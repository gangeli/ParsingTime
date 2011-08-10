package time

import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.ie.NumberNormalizer
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.util._

import org.goobs.database._

import java.util.Properties;

import scala.collection.JavaConversions._

@Table(name="source")
class Source extends DatabaseObject {
	@Key(name="did")
	var did:Int = -1
	@Key(name="name", length=63)
	var name:String = null
	@Key(name="notes")
	var notes:String = null
}

	
case class NumberAnnotator[S <: TimeSentence, T <: TimeTag](
		sentClass:Class[S], tagClass:Class[T]) {
	val did = 30452;
	case class NAnn(start:Int,var len:Int,num:Number,t:String)

	def run = {
		//--Setup
		//(database)
		val db = Database.fromString(
			"psql://research@localhost:data<what?why42?").connect
		val sentences = db.getObjects(sentClass,
			"SELECT * FROM "+Database.getTableName(sentClass))
		//(javanlp pipeline)
		val props = new Properties
		props.setProperty("annotators","tokenize, ssplit, pos, lemma")
		val pipeline = new StanfordCoreNLP(props)
		//--Prepare
		if(db.getFirstObjectWhere(classOf[Source],"did="+did) == null){
			val source = db.emptyObject(classOf[Source])
			source.did = did;
			source.name = "Number Annotation for Time Data"
			source.notes = ""
			source.flush
			println("CREATED SOURCE")
		} else {
			println("FOUND SOURCE")
		}
		val delCount = db.deleteObjectsWhere(tagClass, "did='"+did+"'")
		println("DELETED " + delCount + " tags")
		//--Modify
		sentences.foreach{ (sent:S) => 
			var numbers = List[NAnn]()
			try{ //TODO get rid of me
			//(annotate)
			val (words,pos) = sent.bootstrap
			println("-----")
			val glossText = sent.gloss.replaceAll("/"," / ")
				.replaceAll("""-"""," - ").replaceAll("""\s+"""," ")
			println(glossText)
			val input:Annotation = new Annotation(glossText)
			pipeline.annotate(input)
			val nums = NumberNormalizer.findAndMergeNumbers(input);
			val tokens = input.get[java.util.List[CoreLabel],TokensAnnotation](
				classOf[TokensAnnotation])
			if(tokens.size != words.length){
				println(U.join(tokens.map{ x => x.word }.toArray,"_"))
				throw new IllegalStateException("Length mismatch: " +
					tokens.size + " versus " + words.length + " sent " + sent.sid);
			}
			//(collect numbers: vars)
			var sentPointer = 0
			var numsPointer = 0
			var current:NAnn = null
			//(collect numbers: algorithm)
			while(sentPointer < words.length && numsPointer < nums.size) {
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
					while(!words(sentPointer).equals(word)){
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
				case (e:Exception) => {
					println("TODO: CAUGHT EXCEPTION " + e)
					e.printStackTrace()
				}
			}
			//(save to database)
			numbers.foreach{ (num:NAnn) => 
				//(value tag)
				val start = db.emptyObject(tagClass)
				start.wid = num.start+1
				start.sid = sent.sid
				start.did = did
				start.key = "num"
				start.value = num.num.toString
				//(length tag)
				val len = db.emptyObject(tagClass)
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

object NumberAnnotator {
	def main(args:Array[String]) = {
		if(args.length != 1){
			System.err.println("usage: NumberAnnotator language")
			System.exit(1)
		}
		args(0).toLowerCase match {
		case "timebank" => 
			(new NumberAnnotator(classOf[TimebankSentence],classOf[TimebankTag])).run
		case "english" => 
			(new NumberAnnotator(classOf[EnglishSentence],classOf[EnglishTag])).run
		case _ =>
			System.err.println("Unknown language: " + args(0))
			System.exit(1)
		}
	}
}


