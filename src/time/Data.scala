package time

import java.lang.{Integer => JInt}
import java.util.{List => JList}
import java.util.Calendar
import java.util.Properties;
import java.io.StringReader;

import scala.util.Sorting.quickSort
import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer

import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.util.logging.StanfordRedwoodConfiguration;
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.util.ArrayCoreMap
import edu.stanford.nlp.util.CoreMaps
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.ling.CoreAnnotation
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.time.JodaTimeUtils
import edu.stanford.nlp.ie.NumberNormalizer
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.pipeline._

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
	val NONE, ORDINAL, NUMBER, UNIT, REAL = Value
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
class TimeDataset(val data:Dataset[CoreMapDatum]) {
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

	private def tokens(docI:Int,sentI:Int):Buffer[CoreLabel] = {
		data
			.get(docI)
			.get[JList[CoreMap],SentencesAnnotation](classOf[SentencesAnnotation])
				.get(sentI)
			.get[JList[CoreLabel],TokensAnnotation](classOf[TokensAnnotation])
	}

	def goldTimes:Iterable[(Int,Int,CoreMap)] = {
		var lst = List[(Int,Int,CoreMap)]()
		//--Each Document
		data.zipWithIndex.foreach{ case (datum:CoreMapDatum,docI:Int) => 
			//--Each Sentence
			datum.get[JList[CoreMap],SentencesAnnotation](classOf[SentencesAnnotation])
					.zipWithIndex
					.foreach{ case (sent:CoreMap,sentI:Int) =>
				//--Each Time Expression
				sent.get[JList[CoreMap],TimeExpressionsAnnotation](classOf[TimeExpressionsAnnotation])
						.foreach{ (exp:CoreMap) =>
					lst = (docI,sentI,exp) :: lst
				}
			}
		}
		lst
	}
	
	def goldSpans(train:Boolean,index:Indexing=Indexing()
			):Array[(TimeSent,Temporal,Time)] = {
		goldTimes.map{ case (docI:Int,sentI:Int,expr:CoreMap) =>
			//(get terms)
			val gold = 
				DataLib.array2JodaTime(
					expr.get[Array[String],TimeValueAnnotation](classOf[TimeValueAnnotation])
				)
			val ground = new Time(new DateTime(
				data
					.get(docI)
					.get[Calendar,CalendarAnnotation](classOf[CalendarAnnotation])
					.getTimeInMillis ))
			//(create sentence)
			(	DataLib.mkTimeSent(expr,tokens(docI,sentI),index,train), gold, ground)
		}.toArray
	}
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

	class IsTokenized extends CoreAnnotation[Boolean]{
		def getType:Class[Boolean] = classOf[Boolean]
	}
	class IsNumbered extends CoreAnnotation[Boolean]{
		def getType:Class[Boolean] = classOf[Boolean]
	}


	
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
				Array[String]("RANGE",begin.toString,end.toString)
			case (begin:String,end:DateTime) =>
				Array[String]("RANGE",begin,end.toString)
			case (begin:DateTime,end:String) =>
				Array[String]("RANGE",begin.toString,end)
			case (begin:Period,fuzzy:Boolean) =>
				val opts = new JodaTimeUtils.ConversionOptions
				opts.approximate = true
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
		
	def copyTime(lbl:CoreLabel):CoreMap = {
		val rtn = new ArrayCoreMap(6)
		//(time)
		rtn.set(classOf[OriginalTimeMetaAnnotation],
			lbl.get[TimexMetaInfo,OriginalTimeMetaAnnotation]
				(classOf[OriginalTimeMetaAnnotation]))
		rtn.set(classOf[OriginalTimeTypeAnnotation],
			lbl.get[String,OriginalTimeTypeAnnotation]
				(classOf[OriginalTimeTypeAnnotation]))
		rtn.set(classOf[OriginalTimeValueAnnotation],
			lbl.get[String,OriginalTimeValueAnnotation]
				(classOf[OriginalTimeValueAnnotation]))
		rtn.set(classOf[TimeIdentifierAnnotation],
			lbl.get[String,TimeIdentifierAnnotation]
				(classOf[TimeIdentifierAnnotation]))
		rtn.set(classOf[TimeValueAnnotation],
			lbl.get[Array[String],TimeValueAnnotation]
				(classOf[TimeValueAnnotation]))
		rtn.set(classOf[TimeValueAnnotation],
			lbl.get[Array[String],TimeValueAnnotation]
				(classOf[TimeValueAnnotation]))
		//(return)
		rtn
	}
	def isTimex(lbl:CoreLabel):Boolean
		= lbl.get[String,TimeIdentifierAnnotation](
				classOf[TimeIdentifierAnnotation]) != null
	
	def relinkTimexes(sent:CoreMap):Unit = {
		//--Variables
		val origTokens:Seq[CoreLabel] 
			= sent.get[JList[CoreLabel],OriginalTokensAnnotation](
			classOf[OriginalTokensAnnotation])
		val tokens:Seq[CoreLabel] 
			= sent.get[JList[CoreLabel],TokensAnnotation](
			classOf[TokensAnnotation])
		//--Functions
		def copyTime(lbl:CoreLabel,startIndex:Int):CoreMap = {
			val rtn = DataLib.copyTime(lbl)
			//(old begin)
			rtn.set(classOf[OriginalBeginIndexAnnotation],
				lbl.get[java.lang.Integer,TokenBeginAnnotation]
					(classOf[TokenBeginAnnotation]))
			//(new begin)
			rtn.set(classOf[BeginIndexAnnotation],
				new java.lang.Integer(startIndex))
			//(return)
			rtn
		}
		//--Relink
		val (revTimexes,endedOnTimex)
			= tokens.zipWithIndex.foldLeft(List[CoreMap](),false){ 
				case ((timexes:List[CoreMap],lastTimex:Boolean),
				      (tok:CoreLabel,index:Int)) =>
			if(!lastTimex && isTimex(tok)){
				//(case: start timex)
				(copyTime(tok,index) :: timexes, true)
			} else if(lastTimex && isTimex(tok)){
				//(case: in timex)
				(timexes, true)
			} else if(!lastTimex && !isTimex(tok)){
				//(case: not in timex)
				(timexes, false)
			} else if(lastTimex && !isTimex(tok)){
				//(case: ended timex)
				timexes.head.set(classOf[EndIndexAnnotation],
					new java.lang.Integer(index))
				timexes.head.set(classOf[OriginalEndIndexAnnotation],
					tokens.get(index-1).get[java.lang.Integer,TokenEndAnnotation]
						(classOf[TokenEndAnnotation]))
				(timexes, false)
			} else {
				throw new IllegalStateException("impossible")
			}
		}
		//(last timex)
		if(endedOnTimex){
			revTimexes.head.set(classOf[EndIndexAnnotation],
				new java.lang.Integer(tokens.size))
			revTimexes.head.set(classOf[OriginalEndIndexAnnotation],
				tokens.get(tokens.length-1).get[java.lang.Integer,TokenEndAnnotation]
					(classOf[TokenEndAnnotation]))
		}
		val timexes = revTimexes.reverse
		//--Merge
		//(collapse timexes with gaps in them)
		val filtered = (1 until timexes.length).map{ (i:Int) =>
			val lastTID = timexes(i-1).get[String,TimeIdentifierAnnotation](
				classOf[TimeIdentifierAnnotation])
			val thisTID = timexes(i).get[String,TimeIdentifierAnnotation](
				classOf[TimeIdentifierAnnotation])
			if(lastTID == thisTID){
				timexes(i-1).set(classOf[EndIndexAnnotation],
					timexes(i).get[java.lang.Integer,EndIndexAnnotation]
						(classOf[EndIndexAnnotation]))
				timexes(i-1).set(classOf[OriginalEndIndexAnnotation],
					timexes(i).get[java.lang.Integer,OriginalEndIndexAnnotation]
						(classOf[OriginalEndIndexAnnotation]))
				log("collapsed a timex")
				None
			} else {
				Some(timexes(i))
			}
		}.filter{_.isDefined}.map{ _.get }.toList
		//(append first timex)
		val rtn = if(timexes.length < 2){ timexes } else { timexes(0) :: filtered }
		//--Set
		//(error check)
		val tids = rtn.map{ 
			_.get[String,TimeIdentifierAnnotation](
			classOf[TimeIdentifierAnnotation]) }
		if(tids.length != Set(tids:_*).size){
			throw new IllegalStateException(""+tids.length + " > " + Set(tids:_*).size)
		}
		//(set)
		sent.set(classOf[TimeExpressionsAnnotation],
			seqAsJavaList(rtn))
	}

	private lazy val tokenFact = 
		PTBTokenizer.factory(new CoreLabelTokenFactory(),
			PTBTokenizerAnnotator.DEFAULT_OPTIONS)

	private def tokenize(orig:CoreLabel, str:String,offsetZero:Int,
			index:Int):Array[CoreLabel] = {
		val tokIter = tokenFact.getTokenizer(new StringReader(str))
		var offset = offsetZero
		tokIter.map{ (label:CoreLabel) =>
			//(merge labels)
			label.set(classOf[CharacterOffsetBeginAnnotation], 
				new java.lang.Integer(offset))
			label.set(classOf[CharacterOffsetEndAnnotation], 
				new java.lang.Integer(offset+label.originalText.length))
			offset += label.originalText.length
			//(save token offsets)
			val merged = CoreMaps.merge(orig,label)
			val begin:Int = {
				if(label.get[JInt,TokenBeginAnnotation](classOf[TokenBeginAnnotation]) != null){
					label.get[JInt,TokenBeginAnnotation](classOf[TokenBeginAnnotation])
				} else {
					index
				}
			}
			val end:Int = {
				if(label.get[JInt,TokenEndAnnotation](classOf[TokenEndAnnotation]) != null){
					label.get[JInt,TokenEndAnnotation](classOf[TokenEndAnnotation])
				} else {
					index+1
				}
			}
			merged.set(classOf[TokenBeginAnnotation],new java.lang.Integer(begin))
			merged.set(classOf[TokenEndAnnotation],new java.lang.Integer(end))
			assert(isTimex(orig) == isTimex(merged),
				"timex mismatch on tokenization")
			merged
		}.toArray
	}

	def retokSentence(sent:CoreMap):Unit = {
		//--Retokenize Sentence
		val origTokens=sent.get[java.util.List[CoreLabel],TokensAnnotation](TOKENS)
		val origLength = origTokens.size
		val retok = origTokens.zipWithIndex.foldRight(List[CoreLabel]()){ 
				case ((word:CoreLabel,index:Int),soFar:List[CoreLabel]) =>
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
									tokenize(word,tok.toString,offset,index).toList :::
									tokenize(word,"-",offset+tok.length,index).toList
								} else {
									toks ::: tokenize(word,"-",offset,index).toList
								},
								offset+tok.length+1)
							//(tokenize on /)
							case '/' => (new StringBuilder,
								if(tok.length > 0){
									toks :::
									tokenize(word,tok.toString,offset,index).toList :::
									tokenize(word,"/",offset+tok.length,index).toList
								} else {
									toks ::: tokenize(word,"/",offset,index).toList
								},
								offset+tok.length+1)
							//(tokenize on :)
							case ':' => (new StringBuilder,
								if(tok.length > 0){
									toks :::
									tokenize(word,tok.toString,offset,index).toList :::
									tokenize(word,":",offset+tok.length,index).toList
								} else {
									toks ::: tokenize(word,":",offset,index).toList
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
					tokenize(word,lastTerm.toString,finalOffset,index).toList
				} else { otherTerms }
			newTok ::: soFar
		}
		//(set result)
		sent.set(classOf[OriginalTokensAnnotation],origTokens)
		val jRetok:java.util.List[CoreLabel] = retok
		sent.set(classOf[TokensAnnotation],jRetok)
		//(error check)
		assert(retok(retok.length-1).get[JInt,TokenEndAnnotation](classOf[TokenEndAnnotation])
					== origLength, "Lengths changed!")
		retok.foldLeft(0){ case (last:Int,tok:CoreLabel) =>
			val curr:Int = tok.get[JInt,TokenBeginAnnotation](classOf[TokenBeginAnnotation]) 
			assert(curr == last || curr == last+1,
				"Token offsets jump is fishy: " + last + " -> " + curr)
			curr
		}
		//--Re-Link Timexes
		DataLib.relinkTimexes(sent)
	}

	def retokenize(doc:CoreMap):Unit = {
		if(doc.containsKey[Boolean,IsTokenized](classOf[IsTokenized])){
			return
		}
		val sents=doc.get[java.util.List[CoreMap],SentencesAnnotation](SENTENCES)
		//(for each sentence)
		sents.zipWithIndex.foreach{ case (sent:CoreMap,i:Int) =>
			//(retokenize sentence)
			retokSentence(sent)
		}
		doc.set(classOf[IsTokenized], true)
	}
	
	def findNumbers(sent:CoreMap):Unit = {
		//--Set Numbers
		//(find numbers)
		val nums:JList[CoreMap] = NumberNormalizer.findAndMergeNumbers(sent);
		//(error check)
		nums.foldLeft(-1){ case (lastEnd:Int,map:CoreMap) =>
			val begin = map.get[JInt,TokenBeginAnnotation](classOf[TokenBeginAnnotation])
			val end = map.get[JInt,TokenEndAnnotation](classOf[TokenEndAnnotation])
			assert(lastEnd < 0 || begin <= lastEnd, "invalid jump: " + lastEnd + " to " + begin)
			end
		}
		//(tweak tokens)
		val tokens:JList[CoreLabel] = nums.map{ (map:CoreMap) =>
			val subTokens 
				= map.get[JList[CoreLabel],TokensAnnotation](classOf[TokensAnnotation])
			if(subTokens != null && isTimex(subTokens.get(0))) {
				new CoreLabel(CoreMaps.merge(map,copyTime(subTokens.get(0))))
			} else {
				new CoreLabel(map) 
			}
		}.toList
		//(set tokens)
		sent.set(classOf[TokensAnnotation], tokens )
		//--Relink Timexes
		DataLib.relinkTimexes(sent)
		//--Set Annotation
		tokens.foreach{ (num:CoreLabel) =>
			if(num.originalText == null || num.originalText.equals("")){
				num.setOriginalText(num.word)
			}
		}
	}
			
	def normalizeNumbers(doc:CoreMap):Unit = {
		if(doc.containsKey[Boolean,IsNumbered](classOf[IsNumbered])){
			return
		}
		val sents=doc.get[java.util.List[CoreMap],SentencesAnnotation](SENTENCES)
		//(for each sentence)
		sents.zipWithIndex.foreach{ case (sent:CoreMap,i:Int) =>
			//(normalize numbers)
			findNumbers(sent)
		}
		doc.set(classOf[IsNumbered], true)
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
	def number(lbl:CoreLabel):(NumberType.Value,Int) = {
		//(get annotation)
		val numVal = lbl.get[Number,NumericCompositeValueAnnotation](
			classOf[NumericCompositeValueAnnotation])
		val t = numType(lbl.get[String,NumericCompositeTypeAnnotation](
			classOf[NumericCompositeTypeAnnotation]))
		//(get number)
		if(t == null || t == NumberType.NONE){
			(NumberType.NONE,Int.MinValue)
		} else if(math.floor(numVal.doubleValue) == numVal.doubleValue){
			(t,numVal.doubleValue.toInt)
		} else {
			assert(numVal != null && numVal != null, "Bad number value")
			(NumberType.REAL,numVal.intValue)
		}
	}
	
	private def words(span:Buffer[CoreLabel],index:Indexing,train:Boolean):Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			val (typ,num) = DataLib.number(lbl)
			if(typ != NumberType.NONE){
				index.NUM
			} else {
				if(train){
					index.str2w(lbl.word,false)
				} else {
					index.str2wTest(lbl.word,false)
				}
			}
		}.toArray
	}
	private def pos(span:Buffer[CoreLabel], index:Indexing,train:Boolean):Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			if(train){
				index.str2pos(lbl.tag) 
			} else {
				index.str2posTest(lbl.tag) 
			}
		}.toArray
	}
	private def nums(span:Buffer[CoreLabel]):Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			val (typ,num) = DataLib.number(lbl)
			num
		}.toArray
	}
	private def numTypes(span:Buffer[CoreLabel]):Array[NumberType.Value] = {
		span.map{ (lbl:CoreLabel) => 
			val (typ,num) = DataLib.number(lbl)
			typ
		}.toArray
	}

	def mkTimeSent(expr:CoreMap,tokens:Buffer[CoreLabel],index:Indexing,train:Boolean
			):TimeSent = {
		//(get span)
		val beginIndex:Int = expr.get[JInt,BeginIndexAnnotation](classOf[BeginIndexAnnotation])
		val endIndex:Int = expr.get[JInt,EndIndexAnnotation](classOf[EndIndexAnnotation])
		val span = tokens.slice(beginIndex,endIndex)
		mkTimeSent(span,index,train)
	}
	def mkTimeSent(tokens:Buffer[CoreLabel],index:Indexing,train:Boolean):TimeSent = {
		TimeSent(
			words(tokens,index,train),
			pos(tokens,index,train),
			nums(tokens),
			numTypes(tokens),
			index)
	}
	
	def patchAttribute(typ:String,value:String,ground:DateTime):String = {
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
			case "gigaword" => Gigaword.process(args.slice(1,args.length))
			case _ => err("Invalid dataset: " + args(0)); exit(1)
		}
	}
}
