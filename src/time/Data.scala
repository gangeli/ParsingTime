package time

import java.util.Calendar
import java.util.{List => JList}

import scala.util.Sorting.quickSort
import scala.collection.JavaConversions._

import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.ling.CoreAnnotation
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.util.logging.Redwood.Util._

import org.joda.time.DateTime
import org.joda.time.Period
import org.joda.time.DateTimeZone

import org.goobs.database._
import org.goobs.stanford.CoreMapDatum
import org.goobs.testing.Dataset
import org.goobs.stanford.SerializedCoreMapDataset

//------------------------------------------------------------------------------
// UTILITIES
//------------------------------------------------------------------------------
// -- ENUMERATIONS --
object Language extends Enumeration {
	val english = Value
}
object NumberType extends Enumeration {
	val NONE, ORDINAL, NUMBER, UNIT = Value
}

// -- AUX CLASSES --
case class TimexMetaInfo(doc:String,sentence:Int,word:Int,tid:String)

// -- ANNOTATIONS --
class TimeExpressionsAnnotation extends CoreAnnotation[java.util.List[CoreMap]]{
	def getType:Class[java.util.List[CoreMap]] = classOf[java.util.List[CoreMap]]
}
class TimeValueAnnotation extends CoreAnnotation[Array[String]]{
	def getType:Class[Array[String]] = classOf[Array[String]]
}
class OriginalTimeMetaAnnotation extends CoreAnnotation[TimexMetaInfo]{
	def getType:Class[TimexMetaInfo] = classOf[TimexMetaInfo]
}
class OriginalTimeTypeAnnotation extends CoreAnnotation[String]{
	def getType:Class[String] = classOf[String]
}
class OriginalTimeValueAnnotation extends CoreAnnotation[String]{
	def getType:Class[String] = classOf[String]
}
class TimeIdentifierAnnotation extends CoreAnnotation[String]{
	def getType:Class[String] = classOf[String]
}
class IsTestAnnotation extends CoreAnnotation[Boolean]{
	def getType:Class[Boolean] = classOf[Boolean]
}
class OriginalTokensAnnotation 
		extends CoreAnnotation[java.util.List[CoreLabel]] {
	def getType:Class[java.util.List[CoreLabel]]
		= classOf[java.util.List[CoreLabel]]
}
class OriginalBeginIndexAnnotation
		extends CoreAnnotation[java.lang.Integer]{
	def getType:Class[java.lang.Integer] = classOf[java.lang.Integer]
}
class OriginalEndIndexAnnotation
		extends CoreAnnotation[java.lang.Integer]{
	def getType:Class[java.lang.Integer] = classOf[java.lang.Integer]
}



//------------------------------------------------------------------------------
// DATA
//------------------------------------------------------------------------------
object TimeDataset {
	def main(args:Array[String]) = {
		new TimeDataset(new SerializedCoreMapDataset(
			System.getenv("HOME") + 
				"/workspace/time/aux/coremap/tempeval2-english-retok-numbers"
			)
		).timexes.zipWithIndex.foreach{ case (t:Timex,i:Int) =>
			println(i + ",\"" + t.gloss + "\"")
		}
	}
}
class TimeDataset(data:Dataset[CoreMapDatum]) {
	def slice(minInclusive:Int,maxExclusive:Int):TimeDataset
		= new TimeDataset(data.slice(minInclusive,maxExclusive))

	def slice(isTest:Boolean):TimeDataset = {
		//(variables)
		var minInc = 0
		var maxExc = 0
		var seenAny:Boolean = false
		var seenOpposite:Boolean = false
		var i=0
		//(iterate)
		data.iterator.foreach{ (doc:CoreMapDatum) =>
			val docTest = doc.get[Boolean,IsTestAnnotation](classOf[IsTestAnnotation])
			if(docTest == isTest){
				//((case: in set))
				assert(!seenAny || !seenOpposite,"Interleaved train/test")
				if(!seenAny) {
					minInc = i;
					seenAny = true
				}
				maxExc = i + 1
			} else {
				//((case: not in set))
				if(seenAny){
					seenOpposite = true
				}
			}
			//((increment))
			i += 1
		}
		//(slice)
		slice(minInc,maxExc)
	}

	def train:TimeDataset = slice(false)
	def test:TimeDataset = slice(true)

	def timexes:Array[Timex] = {
		forceTrack("Reading Timexes")
		var index = 0
		//(variables)
		var rtn:List[Timex] = List[Timex]()
		//(get timexes)
		data.iterator.foreach{ (doc:CoreMapDatum) => //for each doc
			val (timex,i) = Timex(doc,index)
			index = i
			rtn = rtn ::: timex
		}
		endTrack("Reading Timexes")
		//(return)
		rtn.toArray
	}
}

object Timex {
	var index:Int = 0
	def apply(doc:CoreMap,i:Int):(List[Timex],Int) = {
		var index = i
		var rtn:List[Timex] = List[Timex]()
		//(pub time)
		val pubTime = Time(new DateTime(
			doc.get[Calendar,CalendarAnnotation](classOf[CalendarAnnotation])
			.getTimeInMillis))
		//(is test)
		val isTest = 
			doc.get[Boolean,IsTestAnnotation](classOf[IsTestAnnotation])
		//(sentences)
		val sents = 
			doc.get[JList[CoreMap],SentencesAnnotation](
			classOf[SentencesAnnotation])
		sents.foreach{ (sent:CoreMap) => //for each sentence
			//(get tokens)
			val tokens:JList[CoreLabel] =
				sent.get[JList[CoreLabel],TokensAnnotation](
				classOf[TokensAnnotation])
			val origTokens:JList[CoreLabel] =
				sent.get[JList[CoreLabel],OriginalTokensAnnotation](
				classOf[OriginalTokensAnnotation])
			assert(tokens != null, "No tokens for " + sent)
			val tokenList:List[CoreLabel] = tokens.map{ x => x }.toList
			val origTokenList:List[CoreLabel] = origTokens.map{ x => x }.toList
			//(get timexes)
			val timexes:JList[CoreMap] = 
				sent.get[java.util.List[CoreMap],TimeExpressionsAnnotation](
				classOf[TimeExpressionsAnnotation])
			//(store timexes)
			if(timexes != null){
				timexes.foreach{ (timex:CoreMap) =>  //for each timex
					val tmx = 
						new Timex(index,timex,origTokenList,tokenList,pubTime,isTest)
					rtn = tmx :: rtn
					index += 1
					log(tmx)
				}
			}
		}
		(rtn.reverse,index)
	}
	def apply(doc:CoreMap):List[Timex] = {
		val (rtn,rtnI) = apply(doc,index)
		index = rtnI
		rtn
	}
}

case class Timex(index:Int,time:CoreMap,origSent:List[CoreLabel],
		sent:List[CoreLabel],pubTime:Time,isTest:Boolean) {
	private val span:List[CoreLabel] = {
		val begin = time.get[java.lang.Integer,BeginIndexAnnotation](
							classOf[BeginIndexAnnotation])
		val end = time.get[java.lang.Integer,EndIndexAnnotation](
							classOf[EndIndexAnnotation])
		val origBegin = time.get[java.lang.Integer,OriginalBeginIndexAnnotation](
							classOf[OriginalBeginIndexAnnotation])
		val origEnd = time.get[java.lang.Integer,OriginalEndIndexAnnotation](
							classOf[OriginalEndIndexAnnotation])
		if(end <= begin){
			warn("Retokenized range is invalid: " + begin + " to " + end)
			assert(origBegin < origEnd, "Range is invalid: " + time)
			origSent.slice(origBegin,origEnd)
		} else {
			sent.slice(begin,end)
		}
	}

	private def numType(str:String):NumberType.Value = {
		if(str != null){
			str match {
				case "ORDINAL" => NumberType.ORDINAL
				case "NUMBER" => NumberType.NUMBER
				case "UNIT" => NumberType.UNIT
				case _ => NumberType.NONE
			}
		} else {
			NumberType.NONE
		}
	}

	def originalType:String = time.get[String,OriginalTimeTypeAnnotation](
			classOf[OriginalTimeTypeAnnotation])
	def originalValue:String = time.get[String,OriginalTimeValueAnnotation](
			classOf[OriginalTimeValueAnnotation])
	
	def filterFromExtent(lines:List[String]):List[String] = {
		//(get extent pattern)
		val meta:TimexMetaInfo = time.get[TimexMetaInfo,OriginalTimeMetaAnnotation](
			classOf[OriginalTimeMetaAnnotation])
		val Regexp = ("""^.*("""+meta.doc+""").*("""+meta.tid+""").*$""").r
		//(filter)
		lines.filter{ (line:String) =>
			line match {
				case Regexp(doc,tid) => false
				case _ => true
			}
		}
	}

	def tempevalAttribute(typ:Option[String],value:Option[String],ground:DateTime,
			padMisses:Boolean):String = {
		val meta:TimexMetaInfo = time.get[TimexMetaInfo,OriginalTimeMetaAnnotation](
			classOf[OriginalTimeMetaAnnotation])
		//(common header)
		def header:StringBuilder = 
			(new StringBuilder)
				.append(meta.doc).append("\t")
				.append(meta.sentence).append("\t")
				.append(meta.word).append("\t")
				.append("timex3").append("\t")
				.append(meta.tid).append("\t")
				.append("1").append("\t")
		//(variables)
		var s:String = ""
		var hasType:Boolean = false
		//(type)
		typ match {
			case Some(typ) => 
				hasType = true
				s += (new StringBuilder)
					.append(header).append("type").append("\t").append(typ).toString
			case None => 
				if(padMisses){
					hasType = true
					s += (new StringBuilder)
						.append(header).append("type").append("\t").append("MISS").toString
				}
		}
		//(value)
		value match {
			case Some(value) => 
				if(hasType){ s += "\n" }
				s += (new StringBuilder)
					.append(header).append("value").append("\t")
					.append(patchAttribute(typ.orNull,value,ground)).toString
			case None => 
				if(padMisses){
					if(hasType){ s += "\n" }
					s += (new StringBuilder)
						.append(header).append("value").append("\t").append("MISS").toString
				}
		}
		//(return)
		s
	}

	private def patchAttribute(typ:String,value:String,ground:DateTime):String = {
		def zeroPad(i:Int,padding:Int):String 
			= "0"*(padding-i.toString.length)+i.toString
		typ match {
			case "TIME" =>
				if(value.matches("T\\d{4}")){ 
					"T" + value.substring(1,3) + ":" + value.substring(3,5) 
				} else if(value.matches("T[0-9]+:[0-9]+")) {
					"" + ground.getYear + "-" +
						zeroPad(ground.getMonthOfYear,2) + "-" +
						zeroPad(ground.getDayOfMonth,2) + value
				} else if(value.matches("T[0-9]+")) {
					"" + ground.getYear + "-" +
						zeroPad(ground.getMonthOfYear,2) + "-" +
						zeroPad(ground.getDayOfMonth,2) + "T" +
						value.substring(1) + ":00"
				} else if(value.equals("T00:00")){
					"" + ground.getYear + "-" +
						zeroPad(ground.getMonthOfYear,2) + "-" +
						zeroPad(ground.getDayOfMonth,2) + "T24"
				} else {
					value
				}
			case "DATE" =>
				if (value.matches("\\d{8}T.*")) {
					value.substring(0,4) + "-"  +
						value.substring(4,6) + "-"  +
						value.substring(6);
				} else if (value.matches("\\d{8}")) {
					value.substring(0,4) + "-"  +
						value.substring(4,6) + "-"  +
						value.substring(6,8);
				} else if (value.matches("\\d\\d\\d\\d..")) {
					value.substring(0,4) + "-"  +
						value.substring(4,6);
				} else if (value.matches("[0-9X]{4}W[0-9X ]{1,2}-?[^-]*")) {
					val Form = """([0-9X]{4})W([0-9X ]{1,2})(-?)([^-]*)""".r
					val Form(year,week,dash,rest) = value
					year + "-W" + 
						{if(week.matches("X+")) week else zeroPad(week.trim.toInt,2)} +
						{if(rest.length > 0){ "-" + rest } else { "" }}
				} else if(value.matches("\\d\\d\\dX")){
					value.substring(0,3)
				} else {
					value
				}
			case "DURATION" => value
			case _ => value
		}
	}

	def tid:Int = index
	def words(test:Boolean):Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			//(get annotation)
			val numVal = lbl.get[Number,NumericCompositeValueAnnotation](
				classOf[NumericCompositeValueAnnotation])
			val t = numType(lbl.get[String,NumericCompositeTypeAnnotation](
				classOf[NumericCompositeTypeAnnotation]))
			val w = if(t != NumberType.NONE){ numVal.toString } else { lbl.word }
			//(get word)
			val wInt = if(test) {
				U.str2wTest(w) 
			} else {
				U.str2w(w)
			}
			wInt
		}.toArray
	}
	def gloss:String = {
		span.map{ (lbl:CoreLabel) => lbl.word }.mkString(" ")
		
	}
	def pos(test:Boolean):Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			if(test) U.str2posTest(lbl.tag) else U.str2pos(lbl.tag)
		}.toArray
	}
	def nums:Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			//(get annotation)
			val numVal = lbl.get[Number,NumericCompositeValueAnnotation](
				classOf[NumericCompositeValueAnnotation])
			val numType = lbl.get[String,NumericCompositeTypeAnnotation](
				classOf[NumericCompositeTypeAnnotation])
			//(get number)
			if(numVal != null){
				numVal.intValue
			} else {
				-1
			}
		}.toArray
	}
	def numTypes:Array[NumberType.Value] = {
		span.map{ (lbl:CoreLabel) => 
			numType(lbl.get[String,NumericCompositeTypeAnnotation](
				classOf[NumericCompositeTypeAnnotation]))
		}.toArray
	}
	def gold:Temporal
		= DataLib.array2JodaTime(
			time.get[Array[String],TimeValueAnnotation](classOf[TimeValueAnnotation]))
	
	def grounding:Time = pubTime

	override def toString:String = "timex["+tid+"] "
}
