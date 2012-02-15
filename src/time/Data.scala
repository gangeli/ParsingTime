package time

import java.util.Calendar
import java.util.{List => JList}
import java.util.Properties;
import java.io.StringReader;

import scala.util.Sorting.quickSort
import scala.collection.JavaConversions._

import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.util.logging.StanfordRedwoodConfiguration;
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.ling.CoreAnnotation
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.time.JodaTimeUtils
import edu.stanford.nlp.ie.NumberNormalizer
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.pipeline.PTBTokenizerAnnotator

import org.joda.time.DateTime
import org.joda.time.Period
import org.joda.time.DateTimeZone

import org.goobs.database._
import org.goobs.stanford.CoreMapDatum
import org.goobs.testing.Dataset
import org.goobs.stanford.SerializedCoreMapDataset
import org.goobs.stanford.JavaNLP._

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
	private def number(lbl:CoreLabel):(NumberType.Value,Int) = {
		//(get annotation)
		val numVal = lbl.get[Number,NumericCompositeValueAnnotation](
			classOf[NumericCompositeValueAnnotation])
		val t = numType(lbl.get[String,NumericCompositeTypeAnnotation](
			classOf[NumericCompositeTypeAnnotation]))
		//(get number)
		if(t == null || t == NumberType.NONE){
			(NumberType.NONE,Int.MinValue)
		} else {
			assert(numVal != null && numVal != Int.MinValue, "Bad number value")
			assert(math.floor(numVal.doubleValue) == numVal.doubleValue, 
				"Number is not an integer: " + numVal)
			(t,numVal.intValue)
		}
	}
	def words(str2w:String=>Int,numTerm:Int):Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			val (typ,num) = number(lbl)
			if(typ != NumberType.NONE){
				numTerm
			} else {
				str2w(lbl.word)
			}
		}.toArray
	}
	def gloss:String = {
		span.map{ (lbl:CoreLabel) => lbl.word }.mkString(" ")
	}
	def pos(pos2w:String=>Int):Array[Int] = { 
		span.map{ (lbl:CoreLabel) => pos2w(lbl.tag) }.toArray
	}
	def nums:Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			val (typ,num) = number(lbl)
			num
		}.toArray
	}
	def numTypes:Array[NumberType.Value] = {
		span.map{ (lbl:CoreLabel) => 
			val (typ,num) = number(lbl)
			typ
		}.toArray
	}
	def gold:Temporal
		= DataLib.array2JodaTime(
			time.get[Array[String],TimeValueAnnotation](classOf[TimeValueAnnotation]))
	
	def grounding:Time = pubTime

	override def toString:String = "timex["+tid+"] "
}

//------------------------------------------------------------------------------
// DATA Processing
//------------------------------------------------------------------------------
object DataLib {
	val Tm = """^T([0-9]{1,2})([0-9]{1,2})?$""".r
	val Year = """^([0-9]{2,4})$""".r
	val YearMonth = """^([0-9]{4})-?([0-9]{1,2})$""".r
	val YearMonthDayHourMin = 
		"""^([0-9]{4})-?([0-9]{1,2})-?([0-9]{1,2})T?([0-9]{1,2})?([0-9]{1,2})?$""".r
	val YearMonthDayTime = 
		"""^([0-9]{4})-?([0-9]{1,2})-?([0-9]{1,2})T?(MO|AF|EV|NI)?$""".r
	val YearWeekWE = """^([0-9]{4})-?W([0-9]{1,2})-?(WE)?$""".r
	val YearQuarter = """^([0-9]{4})-?Q([1-4])$""".r
	val YearHalf = """^([0-9]{4})-?H([1-2])$""".r
	val YearSeason = """^([0-9]{4})-?(SP|SU|FA|WI)$""".r
	val Period = """^P(([0-9]*|X)(D|W|M|Q|Y|E|C|L|H|S|T))+$""".r
	val periodPattern = java.util.regex.Pattern.compile(
		"""([0-9]*|X)(D|W|M|Q|Y|E|C|L|H|S|T)""")
	val Unk = """^(.*X.*)$""".r
	
	def timex2JodaTime(timex:String,ground:DateTime):Any = {
		val str = timex.trim.replaceAll("""\s+""","")
		val pass1 = str match {
			case Tm(hr,min) => 
				val base = ground.withHourOfDay(hr.toInt).
					withMinuteOfHour(if(min == null || min.equals("")) 0 else min.toInt)
				(base, 
					if(min == null || min.equals("")){ base.plusHours(1) }
					else{ base.plusMinutes(1) }
				)
			case Year(y) =>
				val (yr,dur) = y.length match {
					case 2 => (1900 + y.toInt, 1)
					case 3 => (y.toInt*10, 10)
					case 4 => (y.toInt, 1)
				}
				val base = new DateTime(yr.toInt,1,1,0,0,0,0) 
				(base, base.plusYears(dur))
			case YearMonth(year,month) => 
				val base = new DateTime( year.toInt, month.toInt,1,0,0,0,0)
				(base, base.plusMonths(1))
			case YearMonthDayHourMin(year,month,day,hour,min) => 
				val hr = if(hour == null || hour.equals("")){ 0 }else{ hour.toInt-1 }
				val mn = if(min == null || min.equals("")){ 0 }else{ min.toInt }
				val base = 
					new DateTime(year.toInt,month.toInt,day.toInt,hr,mn,0,0)
				(base, 
					if(mn != 0){
						base.plusMinutes(1)
					} else if(hr != 0){
						base.plusHours(1)
					} else {
						base.plusDays(1)
					}
				)
			case YearMonthDayTime(year,month,day,time) => 
				val base = time match {
					case "MO" => new DateTime(year.toInt,month.toInt,day.toInt,8,0,0,0)
					case "AF" => new DateTime(year.toInt,month.toInt,day.toInt,12,0,0,0)
					case "EV" => new DateTime(year.toInt,month.toInt,day.toInt,16,0,0,0)
					case "NI" => new DateTime(year.toInt,month.toInt,day.toInt,20,0,0,0)
				}
				(base, base.plusHours(4))
			case _ => null
		}
		val pass2 = if(pass1 != null) pass1 else str match {
			case YearWeekWE(year,week,we) =>
				val base = we match {
					case "WE" => 
						val b = new DateTime
						b.withYear(year.toInt).
							withWeekOfWeekyear(week.toInt).
							withDayOfWeek(6).
							withMillisOfDay(0)
					case _ => 
						val b = new DateTime
						b.withYear(year.toInt).
							withWeekOfWeekyear(week.toInt).
							withDayOfWeek(1).
							withMillisOfDay(0)
				}
				(base,
					we match{ 
						case "WE" => base.plusDays(2)
						case _ => base.plusWeeks(1)
					}
				)
			case YearQuarter(year,quarter) =>
				val base = new DateTime(year.toInt,(quarter.toInt-1)*3+1,1,0,0,0,0)
				(base,base.plusMonths(3))
			case YearHalf(year,half) =>
				val base = new DateTime(year.toInt,(half.toInt-1)*6+1,1,0,0,0,0)
				(base,base.plusMonths(6))
			case YearSeason(year,season) =>
				val quarter = season match {
					case "WI" => 4
					case "SP" => 1
					case "SU" => 2
					case "FA" => 3
				}
				val base = new DateTime(year.toInt,(quarter.toInt)*3,1,0,0,0,0)
				(base,base.plusMonths(3))
			case _ => null
		}
		val pass3 = if(pass2 != null) pass2 else str match {
			case Period(junk:String,_*) =>
				val matcher = periodPattern.matcher(str)
				var period:Period = new Period
				var seenTime:Boolean = false
				var fuzzy:Boolean = false
				while(matcher.find){
					val numString:String = matcher.group(1)
					val unit:String = matcher.group(2)
					if(numString.equals("")){ 
						seenTime = true 
					} else if(fuzzy){
						throw new IllegalArgumentException("Multiple fuzzy times")
					} else {
						val num:Int = 
							if(numString.equals("X")){ fuzzy = true; 1 }
							else{ numString.toInt }
						period = (unit,seenTime) match {
							case ("L",_) => period.plusYears(1000*num)
							case ("C",_) => period.plusYears(100*num)
							case ("E",_) => period.plusYears(10*num)
							case ("Y",_) => period.plusYears(1*num)
							case ("Q",_) => period.plusMonths(3*num)
							case ("M",false) => period.plusMonths(1*num)
							case ("W",_) => period.plusWeeks(1*num)
							case ("D",_) => period.plusDays(1*num)
							case ("H",_) => period.plusHours(1*num)
							case ("M",true) => period.plusMinutes(1*num)
							case ("S",_) => period.plusSeconds(1*num)
						}
					}
				}
				(period,fuzzy)
			case "PAST_REF" => ("PAST",ground)
			case "FUTURE_REF" => (ground,"FUTURE")
			case "PRESENT_REF" => (ground,ground)
			case Unk(x) => str
			case _ => 
				val base = new DateTime(str)
				(base,base)
		}
		pass3
	}
	
	def jodaTime2Array(time:Any,timex:String):Array[String] = {
		time match {
			case (begin:DateTime,end:DateTime) =>
				val check = JodaTimeUtils.timexDateValue(begin,end)
				if(!check.equals(timex) && !timex.equals("PRESENT_REF")){ 
					err("BAD TRANSlATION: " + timex + "->" + check + "   " + time) 
				}
				Array[String]("RANGE",begin.toString,end.toString)
			case (begin:String,end:DateTime) =>
				Array[String]("RANGE",begin,end.toString)
			case (begin:DateTime,end:String) =>
				Array[String]("RANGE",begin.toString,end)
			case (begin:Period,fuzzy:Boolean) =>
				val check = JodaTimeUtils.timexDurationValue(begin,fuzzy)
				if(!check.equals(timex)){ err("BAD TRANSLATION: " + timex + "->" + check + "   " + time) }
				def mkVal(i:Int) = if(fuzzy && i != 0) "x" else ""+i
				Array[String]("PERIOD",
					mkVal(begin.getYears),
					mkVal(begin.getMonths),
					mkVal(begin.getWeeks),
					mkVal(begin.getDays),
					mkVal(begin.getHours),
					mkVal(begin.getMinutes),
					mkVal(begin.getSeconds) )
			case (s:String) => Array[String]("UNK",s)
			case _ => throw new IllegalArgumentException("Unknown time: " + time)
		}
	}

	def array2JodaTime(timeVal:Array[String]):Temporal = {
		assert(timeVal.length > 0, "No time value for timex!")
		val inType:String = timeVal(0).trim
		inType match {
			case "INSTANT" => {
				//(case: instant time)
				assert(timeVal.length == 2, "Instant has one element")
				if(timeVal(1).trim == "NOW"){
					Range(Duration.ZERO)
				} else {
					val rawTime = new DateTime(timeVal(1).trim)
					val time:Time = 
						if(rawTime.equals(Time.DAWN_OF)){ Time.DAWN_OF }
						else if(rawTime.equals(Time.END_OF)){ Time.END_OF }
						else Time(rawTime)
					Range(time)
				}
			}
			case "RANGE" => {
				//(case: range)
				assert(timeVal.length == 3, "Range has two elements")
				val begin:String = timeVal(1).trim
				val end:String = timeVal(2).trim
				val beginTime:Time
					= if(begin.equals("PAST")) Time.DAWN_OF else Time(new DateTime(begin))
				val endTime:Time
					=if(end.equals("FUTURE")) Time.END_OF else Time(new DateTime(end))
				Range(
						{if(beginTime.equals(Time.DAWN_OF)){ Time.DAWN_OF }
						else if(beginTime.equals(Time.END_OF)){ Time.END_OF }
						else beginTime},

						{if(endTime.equals(Time.DAWN_OF)){ Time.DAWN_OF }
						else if(endTime.equals(Time.END_OF)){ Time.END_OF }
						else endTime}
					)
			}
			case "PERIOD" => {
				//(case: duration)
				assert(timeVal.length == 8, "Period has invalid element count")
				var isApprox = false
				def mkInt(str:String):Int = {
					if(str.equalsIgnoreCase("x")){ isApprox = true; 1 }else{ str.toInt }
				}
				val rtn:Duration = Duration(new Period(
					mkInt(timeVal(1)),
					mkInt(timeVal(2)),
					mkInt(timeVal(3)),
					mkInt(timeVal(4)),
					mkInt(timeVal(5)),
					mkInt(timeVal(6)),
					mkInt(timeVal(7)),
					0
					))
				if(isApprox){ ~rtn } else { rtn }
			}
			case "UNK" => {
				new UnkTime
			}
			case _ => throw new IllegalStateException("Unknown time: " + 
				inType + " for timex: " + timeVal.mkString(" "))
		}
	}

	def relinkTimexes(sent:CoreMap):Unit = {
		//--Variables
		val origTokens = sent.get[JList[CoreLabel],OriginalTokensAnnotation](
			classOf[OriginalTokensAnnotation])
		val retok = sent.get[JList[CoreLabel],TokensAnnotation](
			classOf[TokensAnnotation])
		val timexes:java.util.List[CoreMap] = 
			sent.get[java.util.List[CoreMap],TimeExpressionsAnnotation](
			classOf[TimeExpressionsAnnotation])
		//--Retokenize
		if(timexes != null){
			timexes.foreach{ (timex:CoreMap) =>
				//(get offset information)
				val oldBegin =
					if(timex.has[java.lang.Integer,OriginalBeginIndexAnnotation](
							classOf[OriginalBeginIndexAnnotation])){
						timex.get[java.lang.Integer,OriginalBeginIndexAnnotation](
							classOf[OriginalBeginIndexAnnotation])
					} else {
						timex.get[java.lang.Integer,BeginIndexAnnotation](
							classOf[BeginIndexAnnotation])
					}
				val oldEnd =
					if(timex.has[java.lang.Integer,OriginalEndIndexAnnotation](
							classOf[OriginalEndIndexAnnotation])){
						timex.get[java.lang.Integer,OriginalEndIndexAnnotation](
							classOf[OriginalEndIndexAnnotation])
					} else {
						timex.get[java.lang.Integer,EndIndexAnnotation](
							classOf[EndIndexAnnotation])
					}
				assert(oldBegin != null)
				assert(oldEnd != null, timex.toString)
				val beginOffset = origTokens(oldBegin).beginPosition
				val endOffset = origTokens(oldEnd).beginPosition
				timex.set(classOf[OriginalBeginIndexAnnotation],oldBegin)
				timex.set(classOf[OriginalEndIndexAnnotation],oldEnd)
				//(find new positions)
				retok.zipWithIndex.foreach{ case (tok:CoreLabel,index:Int) => 
					val cand = tok.beginPosition
					if(cand == beginOffset){
						timex.set(classOf[BeginIndexAnnotation],
							new java.lang.Integer(index))
					}
					if(cand == endOffset){
						timex.set(classOf[EndIndexAnnotation],
							new java.lang.Integer(index))
					}
				}
				//(error check)
				if(timex.get[java.lang.Integer,BeginIndexAnnotation](
						classOf[BeginIndexAnnotation]) == null){
					throw new IllegalStateException("No begin index for " + timex)
				}
				if(timex.get[java.lang.Integer,EndIndexAnnotation](
						classOf[EndIndexAnnotation]) == null){
					throw new IllegalStateException("No end index for " + timex)
				}
			}
		}
	}
	

	private lazy val tokenFact = 
		PTBTokenizer.factory(new CoreLabelTokenFactory(),
			PTBTokenizerAnnotator.DEFAULT_OPTIONS)
	private def tokenize(str:String,offsetZero:Int):Array[CoreLabel] = {
		val tokIter = tokenFact.getTokenizer(new StringReader(str))
		var offset = offsetZero
		tokIter.map{ (label:CoreLabel) =>
			label.set(classOf[CharacterOffsetBeginAnnotation], 
				new java.lang.Integer(offset))
			label.set(classOf[CharacterOffsetEndAnnotation], 
				new java.lang.Integer(offset+label.originalText.length))
				offset += label.originalText.length
			label
		}.toArray
	}

	def retokSentence(sent:CoreMap):Unit = {
		//--Retokenize Sentence
		val origTokens=sent.get[java.util.List[CoreLabel],TokensAnnotation](TOKENS)
		val retok = origTokens.foldRight(List[CoreLabel]()){ 
				(word:CoreLabel,soFar:List[CoreLabel]) =>
			val orig = word.originalText
			val baseOffset = word.beginPosition
			val finalOffsetGold = word.endPosition
			val (lastTerm,otherTerms,finalOffset)
				= orig.toCharArray.foldLeft(
					(new StringBuilder,List[CoreLabel](),baseOffset)){
					case ((tok:StringBuilder,toks:List[CoreLabel],offset:Int),chr:Char) =>
						chr match {
							//(tokenize on -)
							case '-' => (new StringBuilder,
								if(tok.length > 0){
									toks :::
									tokenize(tok.toString,offset).toList :::
									tokenize("-",offset+tok.length).toList
								} else {
									toks ::: tokenize("-",offset).toList
								},
								offset+tok.length+1)
							//(tokenize on /)
							case '/' => (new StringBuilder,
								if(tok.length > 0){
									toks :::
									tokenize(tok.toString,offset).toList :::
									tokenize("/",offset+tok.length).toList
								} else {
									toks ::: tokenize("/",offset).toList
								},
								offset+tok.length+1)
							//(tokenize on /)
							case ':' => (new StringBuilder,
								if(tok.length > 0){
									toks :::
									tokenize(tok.toString,offset).toList :::
									tokenize(":",offset+tok.length).toList
								} else {
									toks ::: tokenize(":",offset).toList
								},
								offset+tok.length+1)
							//(part of a token)
							case _ => (tok.append(chr),toks,offset)
						}
				}	
			assert(finalOffset+lastTerm.length == finalOffsetGold, 
				"Offset mismatch for word: "+word + " orig: " + word)
			val newTok
				= if(lastTerm.length > 0) {
					otherTerms :::
					tokenize(lastTerm.toString,finalOffset).toList
				} else { otherTerms }
			newTok ::: soFar
		}
		//(set result)
		sent.set(classOf[OriginalTokensAnnotation],origTokens)
		val jRetok:java.util.List[CoreLabel] = retok
		sent.set(classOf[TokensAnnotation],jRetok)
		//--Re-Link Timexes
		DataLib.relinkTimexes(sent)
	}

	def retokenize(doc:CoreMap):Unit = {
		val sents=doc.get[java.util.List[CoreMap],SentencesAnnotation](SENTENCES)
		//(for each sentence)
		sents.foreach{ case (sent:CoreMap) =>
			//(retokenize sentence)
			retokSentence(sent)
		}
	}
	
	def findNumbers(sent:CoreMap):Unit = {
		//(set numbers)
		val nums:JList[CoreMap] = NumberNormalizer.findAndMergeNumbers(sent);
		val tokens:JList[CoreLabel] = nums.map{ new CoreLabel(_) }.toList
		sent.set(classOf[TokensAnnotation], tokens )
		//(relink timexes)
		DataLib.relinkTimexes(sent)
		//(set current annotation)
		tokens.foreach{ (num:CoreLabel) =>
			if(num.originalText == null || num.originalText.equals("")){
				num.setOriginalText(num.word)
			}
		}
	}
			
	def normalizeNumbers(doc:CoreMap):Unit = {
		val sents=doc.get[java.util.List[CoreMap],SentencesAnnotation](SENTENCES)
		//(for each sentence)
		sents.foreach{ case (sent:CoreMap) =>
			//(normalize numbers)
			findNumbers(sent)
		}
	}
}

object Data {
	def main(args:Array[String]):Unit = {	
		val props = new Properties();
		props.setProperty("log.neatExit", "true");
		props.setProperty("log.collapse", "exact");
		StanfordRedwoodConfiguration.apply(props);

		DateTimeZone.setDefault(DateTimeZone.UTC);
		if(args.length < 1){ err("No dataset given"); exit(1) }
		args(0).toLowerCase match {
			case "tempeval2" => TempEval2.normalize(args(1),args(2))
//			case "tempeval2" => mkTempEval(args.slice(1,args.length))
			case "gigaword" => Gigaword.process(args.slice(1,args.length))
			case _ => err("Invalid dataset: " + args(0)); exit(1)
		}
	}
}
