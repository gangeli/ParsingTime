package time

import scala.util.Sorting.quickSort

import org.joda.time.DateTime
import org.joda.time.Period
import org.joda.time.DateTimeZone

import org.goobs.database._
import org.goobs.exec.Log._

import Conversions._

@Table(name="timebank_doc")
class TimebankDocument extends org.goobs.testing.Datum{
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
	def grounding:Time = new Time(new DateTime(pubTime.trim),null,null)

	override def getID = fid
	override def toString:String = filename
}

@Table(name="timebank_sent")
class TimebankSentence extends DatabaseObject with Ordered[TimebankSentence]{
	@PrimaryKey(name="sid")
	private var sid:Int = 0
	@Key(name="fid")
	private var fid:Int = 0
	@Key(name="length")
	private var length:Int = 0
	@Key(name="gloss")
	private var gloss:String = null
	@Child(localField="sid", childField="sid")
	private var tags:Array[TimebankTag] = null
	@Child(localField="sid", childField="sid")
	var timexes:Array[Timex] = null
	var words:Array[Int] = null
	var pos:Array[Int] = null
	
	def init:Unit = { 
		refreshLinks; 
		quickSort(timexes);
		words = new Array[Int](length)
		pos = new Array[Int](length)
		for( i <- 0 until tags.length ){
			tags(i).key match {
				case "form" =>
					words(tags(i).wid-1) = U.str2w(tags(i).value)
				case "pos" =>
					pos(tags(i).wid-1) = U.str2pos(tags(i).value)
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
	private var sid:Int = 0
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
	@Key(name="temporal_function")
	private var temporalFunction:Boolean = false
	@Key(name="mod")
	private var mod:String = null
	@Key(name="gloss")
	private var gloss:String = null

	private var timeCache:Any = null
	var grounding:Time = null
	private var wordArray:Array[Int] = null
	private var posArray:Array[Int] = null

	def setWords(s:TimebankSentence):Timex = {
		wordArray = s.words.slice(scopeBegin,scopeEnd)
		posArray = s.pos.slice(scopeBegin,scopeEnd)
		this
	}
	def words:Array[Int] = wordArray
	def pos:Array[Int] = posArray
	def ground(t:Time):Timex = { grounding = t; this }
	def gold:Any = {
		if(timeCache == null){
			assert(timeVal.length > 0, "No time value for timex " + tid + "!")
			val inType:String = timeVal(0).trim
			timeCache = inType match {
				case "INSTANT" => {
					//(case: instant time)
					assert(timeVal.length == 2, "Instant has one element")
					if(timeVal(1).trim == "NOW"){
						new Time(null,null,null)
					} else {
						new Time(new DateTime(timeVal(1).trim),null,null)
					}
				}
				case "RANGE" => {
					//(case: range)
					assert(timeVal.length == 3, "Range has two elements")
					val begin:String = timeVal(1).trim
					val end:String = timeVal(2).trim
					if(begin == "x" || end == "x"){
						//(case: unbounded range)
						if(begin == "x") assert(end == "NOW", "assumption")
						if(end == "x") assert(begin == "NOW", "assumption")
						if(begin == "x"){
							(r:Range) => Range(r.begin,Time(null,null,null))
						} else if(end == "x"){
							(r:Range) => Range(Time(null,null,null), r.end)
						} else {
							throw fail("Should not reach here")
						}
					} else {
						//(case: normal range)
						Range(
							{if(begin == "NOW") new Time(null,null,null) 
								else new Time(new DateTime(begin),null,null)},
							{if(end == "NOW") new Time(null,null,null) 
								else new Time(new DateTime(end),null,null)} )
					}
				}
				case "PERIOD" => {
					assert(timeVal.length == 8, "Period has 4 elements")
					//(case: duration)
					new Duration(new Period(
						Integer.parseInt(timeVal(1)),
						Integer.parseInt(timeVal(2)),
						Integer.parseInt(timeVal(3)),
						Integer.parseInt(timeVal(4)),
						Integer.parseInt(timeVal(5)),
						Integer.parseInt(timeVal(6)),
						Integer.parseInt(timeVal(7)),
						0
						))
				}
				case "UNK" => {
					new UNK
				}
				case _ => throw new IllegalStateException("Unknown time: " + 
					inType + " for timex: " + this)
			}
		}
		timeCache
	}

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

@Table(name="timebank_tlink")
class TLink extends DatabaseObject with Ordered[TLink]{
	@PrimaryKey(name="lid")
	private var lid:Int = 0
	@Key(name="fid")
	private var fid:Int = 0
	@Key(name="source")
	private var sourceTimexId:Int = 0
	@Key(name="target")
	private var targetTimexId:Int = 0
	@Key(name="type")
	private var linkType:String = null

	override def compare(t:TLink):Int = this.lid - t.lid
	override def toString:String =""+sourceTimexId+"-"+linkType+"->"+targetTimexId
	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[TLink]){
			val other:TLink = o.asInstanceOf[TLink]
			other.lid == this.lid
		}else{
			false
		}
	}
	override def hashCode:Int = lid
}



