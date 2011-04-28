package time

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
	@Key(name="notes")
	private var notes:String = null
	@Child(localField="fid", childField="fid")
	var sentences:Array[TimebankSentence] = null
	@Child(localField="fid", childField="fid")
	var links:Array[TLink] = null
	override def getID = fid
}

@Table(name="timebank_sent")
class TimebankSentence extends DatabaseObject{
	@PrimaryKey(name="sid")
	private var sid:Int = 0
	@Key(name="fid")
	private var fid:Int = 0
	@Key(name="length")
	private var length:Int = 0
	@Key(name="gloss")
	private var gloss:String = null
//	@Child(localField="sid", childField="sid")
//	private var tags:Array[TimebankTag] = null
	@Child(localField="sid", childField="sid")
	var timexes:Array[Timex] = null

	override def toString:String = gloss
}

@Table(name="timebank_tag")
class TimebankTag extends DatabaseObject{
	@Key(name="wid")
	private var wid:Int = 0
	@Key(name="sid")
	private var sid:Int = 0
	@Key(name="did")
	private var did:Int = 0
	@Key(name="key")
	private var key:String = null
	@Key(name="value")
	private var value:String = null
}

@Table(name="timebank_timex")
class Timex extends DatabaseObject{
	@PrimaryKey(name="tid")
	private var tid:Int = 0
	@Key(name="sid")
	private var sid:Int = 0
	@Key(name="scopeBegin")
	private var scopeBegin:Int = 0
	@Key(name="scopeEnd")
	private var scopeEnd:Int = 0
	@Key(name="type")
	private var timeType:String = null
	@Key(name="value")
	private var timeVal:Array[String] = null
	@Key(name="temporalFunction")
	private var temporalFunction:Boolean = false
	@Key(name="functionInDocument")
	private var functionInDocument:String = null
	@Key(name="mod")
	private var mod:String = null
	@Key(name="gloss")
	private var gloss:String = null

	private var timeCache:Any = null

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
					assert(timeVal.length == 5, "Period has 4 elements")
					//(case: duration)
					new Period(
						Integer.parseInt(timeVal(1)),
						Integer.parseInt(timeVal(2)),
						Integer.parseInt(timeVal(3)),
						Integer.parseInt(timeVal(4)),
						0,0,0,0
						)
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

	override def toString:String = {
		"" + tid + "["+scopeBegin+"-"+scopeEnd+"]: " + gloss
	}
	override def equals(other:Any):Boolean = {
		return other.isInstanceOf[Timex] && other.asInstanceOf[Timex].tid == tid
	}
	override def hashCode:Int = tid
}

@Table(name="timebank_tlink")
class TLink extends DatabaseObject{
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
}



