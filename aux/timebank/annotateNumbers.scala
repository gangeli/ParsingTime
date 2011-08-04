#!/bin/bash
exec scala -cp ../../etc/models.jar:../../lib/joda-time.jar:../../etc/lib.jar:../../etc/postgresql.jar:$JAVANLP_HOME/projects/core/classes:$JAVANLP_HOME/projects/core/lib/xom-1.2.6.jar $0 $@ 
!#

object Numbers {
//------------------------------------------------------------------------------
// DATA
//------------------------------------------------------------------------------
import org.goobs.database._
import scala.util.Sorting.quickSort
@Table(name="timebank_doc")
class TimebankDocument extends org.goobs.testing.Datum {
	@PrimaryKey(name="fid")
	private var fid:Int = 0
	@Key(name="filename")
	private var filename:String = null
	@Key(name="pub_time")
	private var pubTime:String = null
	@Key(name="notes")
	private var notes:String = null
	@Child(localField="fid", childField="fid")
	var sentences:Array[TimebankSentence] = null
	
	def init:Unit = { refreshLinks; quickSort(sentences); }

	override def getID = fid
	override def toString:String = filename
}

@Table(name="timebank_sent")
class TimebankSentence extends DatabaseObject with Ordered[TimebankSentence]{
	@PrimaryKey(name="sid")
	var sid:Int = 0
	@Key(name="fid")
	private var fid:Int = 0
	@Key(name="length")
	var length:Int = 0
	@Key(name="gloss")
	var gloss:String = null
	@Child(localField="sid", childField="sid")
	private var tags:Array[TimebankTag] = null
	@Child(localField="sid", childField="sid")
	var timexes:Array[Timex] = null
	var words:Array[String] = null
	var pos:Array[String] = null
	
	def init:Unit = { 
		refreshLinks; 
		quickSort(timexes);
		words = new Array[String](length)
		pos = new Array[String](length)
		for( i <- 0 until tags.length ){
			tags(i).key match {
				case "form" =>
					words(tags(i).wid-1) = tags(i).value
				case "pos" =>
					pos(tags(i).wid-1) = tags(i).value
				case _ => 
					//do nothing
			}
		}
	}

	override def compare(t:TimebankSentence):Int = this.sid - t.sid
	override def toString:String = gloss
}

@Table(name="timebank_tag")
class TimebankTag extends DatabaseObject{
	@Key(name="wid")
	var wid:Int = 0
	@Key(name="sid")
	var sid:Int = 0
	@Key(name="did")
	var did:Int = 0
	@Key(name="key")
	var key:String = null
	@Key(name="value")
	var value:String = null
}

@Table(name="timebank_timex")
class Timex extends DatabaseObject with Ordered[Timex]{
	@PrimaryKey(name="tid")
	var tid:Int = 0
	@Key(name="sid")
	private var sid:Int = 0
	@Key(name="scope_begin")
	private var scopeBegin:Int = 0
	@Key(name="scope_end")
	private var scopeEnd:Int = 0
	@Key(name="type")
	private var timeType:String = null
	@Key(name="value")
	private var timeVal:Array[String] = null
	@Key(name="original_value")
	private var originalValue:String = null
	@Key(name="gloss")
	private var gloss:String = null

	private var timeCache:Any = null
	private var wordArray:Array[String] = null
	private var posArray:Array[String] = null

	def setWords(s:TimebankSentence):Timex = {
		wordArray = s.words.slice(scopeBegin,scopeEnd)
		posArray = s.pos.slice(scopeBegin,scopeEnd)
		this
	}
	def words:Array[String] = wordArray
	def pos:Array[String] = posArray

	override def compare(t:Timex):Int = this.tid - t.tid
	override def toString:String = {
		"" + tid + "["+scopeBegin+"-"+scopeEnd+"]: " +
			gloss.replaceAll("""\n+"""," ")
	}
	override def equals(other:Any):Boolean = {
		return other.isInstanceOf[Timex] && other.asInstanceOf[Timex].tid == tid
	}
	override def hashCode:Int = tid
}

@Table(name="timebank_timextag")
case class TimexTag(
	@Key(name="tid")
	tid:Int,
	@Key(name="did")
	did:Int,
	@Key(name="key", length=31)
	key:String,
	@Key(name="value", length=63)
	value:String
	) extends DatabaseObject

@Table(name="source")
class Source extends DatabaseObject {
	@Key(name="did")
	var did:Int = -1
	@Key(name="name", length=63)
	var name:String = null
	@Key(name="notes")
	var notes:String = null
}

//------------------------------------------------------------------------------
// MAIN
//------------------------------------------------------------------------------
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.ie.NumberNormalizer
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.util._


import java.util.Properties;

import scala.collection.JavaConversions._
	
	val did = 30452;
	case class NAnn(start:Int,var len:Int,num:Number,t:String)

	def main(args:Array[String]) = {
		//--Setup
		//(database)
		val db = Database.fromString(
			"psql://research@localhost:data<what?why42?").connect
		val sentences = db.getObjects(classOf[TimebankSentence],
			"SELECT * FROM timebank_sent")
		//(javanlp pipeline)
		val props = new Properties
		props.setProperty("annotators","tokenize, ssplit, pos, lemma")
		val pipeline = new StanfordCoreNLP(props)
		//--Prepare
		if(db.getFirstObjectWhere(classOf[Source],"did="+did) == null){
			val source = db.emptyObject(classOf[Source])
			source.did = did;
			source.name = "Timebank Number Annotation"
			source.notes = ""
			source.flush
			println("CREATED SOURCE")
		} else {
			println("FOUND SOURCE")
		}
		val delCount = db.deleteObjectsWhere(classOf[TimebankTag], "did='"+did+"'")
		println("DELETED " + delCount + " tags")
		//--Modify
		sentences.foreach{ (sent:TimebankSentence) => 
			var numbers = List[NAnn]()
			try{ //TODO get rid of me
			//(annotate)
			sent.init
			println("-----")
			val glossText = sent.gloss.replaceAll("/"," / ")
				.replaceAll("""-"""," - ").replaceAll("""\s+"""," ")
			println(glossText)
			val input:Annotation = new Annotation(glossText)
			pipeline.annotate(input)
			val nums = NumberNormalizer.findAndMergeNumbers(input);
			val tokens = input.get[java.util.List[CoreLabel],TokensAnnotation](
				classOf[TokensAnnotation])
			if(tokens.size != sent.words.length){
				throw new IllegalStateException("Length mismatch: " +
					tokens.size + " versus " + sent.words.length + " sent " + sent.sid);
			}
			//(collect numbers: vars)
			var sentPointer = 0
			var numsPointer = 0
			var current:NAnn = null
			//(collect numbers: algorithm)
			while(sentPointer < sent.words.length && numsPointer < nums.size) {
				//(sync positions)
				val term = nums(numsPointer)
				val word = term.get[String,TextAnnotation](classOf[TextAnnotation])
				//(get number)
				val num = term.get[Number,NumericCompositeValueAnnotation](
					classOf[NumericCompositeValueAnnotation])
				var t = term.get[String,NumericCompositeTypeAnnotation](
					classOf[NumericCompositeTypeAnnotation])
				//(add any last annotation)
				if(current != null){
					while(!sent.words(sentPointer).equals(word)){
						sentPointer += 1
						if(sentPointer >= sent.length){ 
							throw new IllegalStateException(
								"unbound on " + current + " waiting for '" + word + "'") 
						}
					}
					current.len = sentPointer - current.start
					numbers = current :: numbers
					current = null
				}
				//(prepare new annotation)
				if(num != null){
					if(t == null) {
						throw new IllegalStateException("no type for num: " + num) 
					}
					current = NAnn(sentPointer,-1,num,t)
				}
				sentPointer += 1
				numsPointer += 1
			}
			//(add any dangling annotations)
			if(current != null){
				current.len = sentPointer - current.start
				numbers = current :: numbers
				current = null
			}
			} catch { //TODO get rid of me
				case (e:Exception) => println("TODO: CAUGHT EXCEPTION " + e)
			}
			//(save to database)
			numbers.foreach{ (num:NAnn) => 
				//(value tag)
				val start = db.emptyObject(classOf[TimebankTag])
				start.wid = num.start+1
				start.sid = sent.sid
				start.did = did
				start.key = "num"
				start.value = num.num.toString
				//(length tag)
				val len = db.emptyObject(classOf[TimebankTag])
				len.wid = num.start+1
				len.sid = sent.sid
				len.did = did
				len.key = "num_length"
				len.value = num.len.toString
				//(save)
				start.flush
				len.flush
				println("  flushed " + num)
			}
		}
	}
}
