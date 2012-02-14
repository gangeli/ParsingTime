package time

import java.util.Properties;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.{List => JList};
import java.util.ArrayList;
import java.io._;

import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import scala.io.Source.fromFile

import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.ie.NumberNormalizer
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.util._
import edu.stanford.nlp.time.{Timex => StanfordTimex}
import edu.stanford.nlp.time.GUTimeAnnotator
import edu.stanford.nlp.tagger.maxent.MaxentTagger
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.process.WhitespaceTokenizer
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.time.TimeAnnotations._
import edu.stanford.nlp.time.JodaTimeUtils
import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.util.logging.StanfordRedwoodConfiguration;

import org.goobs.database._
import org.goobs.testing.Dataset
import org.goobs.testing.Datum
import org.goobs.stanford.JavaNLP._
import org.goobs.stanford.Task
import org.goobs.stanford.CoreMapDatum
import org.goobs.stanford.SerializedCoreMapDataset

import org.joda.time._

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
// LIBRARIES
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
									TempEval2Task.tokenize(tok.toString,offset).toList :::
									TempEval2Task.tokenize("-",offset+tok.length).toList
								} else {
									toks ::: TempEval2Task.tokenize("-",offset).toList
								},
								offset+tok.length+1)
							//(tokenize on /)
							case '/' => (new StringBuilder,
								if(tok.length > 0){
									toks :::
									TempEval2Task.tokenize(tok.toString,offset).toList :::
									TempEval2Task.tokenize("/",offset+tok.length).toList
								} else {
									toks ::: TempEval2Task.tokenize("/",offset).toList
								},
								offset+tok.length+1)
							//(tokenize on /)
							case ':' => (new StringBuilder,
								if(tok.length > 0){
									toks :::
									TempEval2Task.tokenize(tok.toString,offset).toList :::
									TempEval2Task.tokenize(":",offset+tok.length).toList
								} else {
									toks ::: TempEval2Task.tokenize(":",offset).toList
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
					TempEval2Task.tokenize(lastTerm.toString,finalOffset).toList
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
//		//(dump numbers -- debug)
//		tokens.foreach{ (num:CoreLabel) =>
//			val numVal = num.get[Number,NumericCompositeValueAnnotation](
//				classOf[NumericCompositeValueAnnotation])
//			val numType = num.get[String,NumericCompositeTypeAnnotation](
//				classOf[NumericCompositeTypeAnnotation])
//			val token = num.word
//			if(numVal != null || numType != null){
//				println(numVal+"\t"+numType+"\t"+
//					{if(numVal == null) "null" else numVal.getClass}+
//					"\t"+token)
//			}
//		}
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

//------------------------------------------------------------------------------
// GUTime Data Processor
// TODO don't hard code tagger paths
//------------------------------------------------------------------------------

object Gigaword {
	val date = """.*id\="[^0-9]+([0-9]{4})([0-9]{2})([0-9]{2})\.[0-9]+".*""".r
	val name = """.*id\="([^"]+)".*""".r

	val pipeline:AnnotationPipeline = {
			val props = new Properties
			props.setProperty("pos.model",
				System.getenv("HOME") +
					"/lib/data/bidirectional-distsim-wsj-0-18.tagger")
			props.setProperty("annotators","tokenize, ssplit, pos")
			val pipe = new StanfordCoreNLP(props);
			pipe
		}

	val tagger = new MaxentTagger(
		System.getenv("HOME") + "/lib/data/bidirectional-distsim-wsj-0-18.tagger")

	def appendDoc(outDir:File,
			f:File, hdr:String, sentsGloss:Array[String]):Unit = {
		startTrack("Document " + hdr)
		//--Create Document
		//(gloss)
		val docGloss:String = 
				sentsGloss.foldLeft(new StringBuilder){case (b:StringBuilder,l:String)=>
					b.append(l).append("\n"); b }.toString 
		//(process header)
		val date(year,month,day) = hdr
		val name(filename) = hdr
		val pubTime = new DateTime(year.toInt,month.toInt,day.toInt,0,0,0,0)
		//(debug)
		log("Processing " + filename)
		//--Create CoreMap
		//(create)
		val doc:Annotation = new Annotation(docGloss)
		//(set calendar)
		val ground:DateTime = new DateTime(year.toInt,month.toInt,day.toInt,0,0,0,0)
		doc.set(classOf[CalendarAnnotation], 
			ground.toGregorianCalendar.asInstanceOf[Calendar])
		//(annotate)
		pipeline.annotate(doc)
		log("(JavaNLP annotated)")
		//(gutime annotation)
		val gutime = new GUTimeAnnotator(new File(
			System.getenv("HOME")+"/workspace/time/etc/"))
		gutime.annotate(doc)
		log("(gutime annotated)")
		//--Iterate Timexes
		//(variables)
		var tid:Int = 0
		var sI:Int = 0
		var wI:Int = 0
		val sentsJ:JList[CoreMap] = doc.get[JList[CoreMap],SentencesAnnotation](
			classOf[SentencesAnnotation])
		val sents:Array[CoreMap] = sentsJ.toList.toArray
		val tokens:Array[Array[CoreLabel]] = sents.map{ (c:CoreMap) =>
				val j:JList[CoreLabel] =
					c.get[JList[CoreLabel],TokensAnnotation](classOf[TokensAnnotation])
				j.toList.toArray
			}
		//((serialization crap))
		doc.set[JList[CoreMap],JList[CoreMap],SentencesAnnotation](
			classOf[SentencesAnnotation],new ArrayList(sents.toList))
		(0 until sents.length).foreach{ (sid:Int) => 
			val j:JList[CoreLabel] = sents(sid).get[JList[CoreLabel],
				TokensAnnotation](classOf[TokensAnnotation])
			val s:List[CoreLabel] = j.toList
			val out:JList[CoreLabel] = new ArrayList(s)
			sents(sid).set(classOf[TokensAnnotation],out)
		}
		//((timexes))
		var timexes:List[CoreMap] = List[CoreMap]()
		//(iterate)
		startTrack("Timexes")
		doc.get[
				JList[CoreMap],TimexAnnotations](
				classOf[TimexAnnotations]).foreach{ case (map:CoreMap) =>
			try {
				//((get vars))
				val begin:Int 
					= map.get[java.lang.Integer,CharacterOffsetBeginAnnotation](
						classOf[CharacterOffsetBeginAnnotation])
				val end:Int 
					= map.get[java.lang.Integer,CharacterOffsetEndAnnotation](
						classOf[CharacterOffsetEndAnnotation])
				val timex:StanfordTimex = map.get[StanfordTimex,TimexAnnotation](
					classOf[TimexAnnotation])
				//((find beginning))
				while(tokens(sI)(wI).beginPosition < begin){
					wI += 1
					if(wI >= tokens(sI).length){ sI += 1; wI = 0 }
				}
				if(tokens(sI)(wI).beginPosition != begin){
					throw new IllegalStateException(
						"Begin is not on a word boundary: "+begin+" : " + tokens(sI)(wI))
				}
				val beginIndex = wI
				//((find end))
				while(tokens(sI)(wI).endPosition < end){
					wI += 1
					if(wI >= tokens(sI).length){ 
						throw new IllegalStateException("Timex crosses sentence boundary")
					}
				}
				if(tokens(sI)(wI).endPosition != end){
					throw new IllegalStateException(
						"End is not on a word boundary: "+end+" : " + tokens(sI)(wI))
				}
				val endIndex = wI+1
				//(process timex)
				//((vars))
				val timexValue = timex.value
				val timexType = timex.timexType
				if(timexValue != null){
					val timex:CoreMap = new ArrayCoreMap(4)
					//((fields))
					timex.set(classOf[TimeIdentifierAnnotation], "t"+tid)
					tid += 1
					timex.set(classOf[BeginIndexAnnotation], 
						new java.lang.Integer(beginIndex))
					timex.set(classOf[EndIndexAnnotation],
						new java.lang.Integer(endIndex))
					timex.set(classOf[OriginalTimeValueAnnotation], timexValue)
					timex.set(classOf[OriginalTimeTypeAnnotation], timexType)
					timex.set(classOf[TimeValueAnnotation], 
						DataLib.jodaTime2Array(
							DataLib.timex2JodaTime(timexValue,ground),
							timexValue)
						)
					//((save))
					if(!sents(sI).has[JList[CoreMap],TimeExpressionsAnnotation](
							classOf[TimeExpressionsAnnotation])){
						sents(sI).set(classOf[TimeExpressionsAnnotation],
							new ArrayList[CoreMap]().asInstanceOf[JList[CoreMap]])
					}
					val timexes = sents(sI).get[JList[CoreMap],TimeExpressionsAnnotation](
							classOf[TimeExpressionsAnnotation])
					timexes.add(timex)
					//((debug))
					log("added: " + 
						tokens(sI).slice(beginIndex,endIndex).map{ _.word }.mkString(" ") + 
						" ["+timexValue+"]")
				}
			} catch {
				case (e:RuntimeException) =>
					err("FAILED: " + e.getMessage)
			}
		}
		endTrack("Timexes")
		//(post-process)
		log("(tokenizing)")
		DataLib.retokenize(doc)
		log("(normalizing numbers)")
		DataLib.normalizeNumbers(doc)
		//(removing GuTime Timexes)
		log("(cleaning up)")
		doc.remove[StanfordTimex,TimexAnnotation](classOf[TimexAnnotation])
		doc.remove[JList[CoreLabel],TokensAnnotation](classOf[TokensAnnotation])
		doc.remove[String,TextAnnotation](classOf[TextAnnotation])
		sents.foreach{ (sent:CoreMap) => 
			sent.remove[JList[CoreMap],TimexAnnotations](classOf[TimexAnnotations])
		}
		//(write)
		log("(writing file)")
		val outFile = outDir.getPath + "/" + filename + ".coremap"
		try {
			IOUtils.writeObjectToFile(doc,outFile)
//			val fos = new FileOutputStream(outFile);
//			val out = new ObjectOutputStream(fos);
//			out.writeObject(doc);
//			out.close();
		} catch {
			case (e:IOException) => {
				log(e)
				new File(outFile).delete
				throw new RuntimeException(e);
			}
		}
		//(done)
		log("(ending track)")
		endTrack("Document " + hdr)
	}

	def appendFile(outDir:File,f:File):(Int,Int) = {
		import scala.io.Source.fromFile
		var lastDoc:String = ""
		var inSent:Boolean = false
		var sent:String = ""
		var sents:List[String] = List[String]()
		var success:Int = 0
		var total:Int = 0
		fromFile(f).getLines.foreach{ case (line:String) =>
			if(line.startsWith("<DOC id")){
				lastDoc = line
			} else{
				line.trim.toLowerCase match {
					case "<s>" => assert(!inSent); inSent = true; sent = ""
					case "</s>" => 
						assert(inSent); inSent = false; 
						sents = sent.replaceAll("""\s+"""," ") :: sents
					case "<text>" => //noop
					case "</text>" => //noop
					case "</doc>" => 
						try{
						appendDoc(outDir,f,lastDoc,sents.reverse.toArray); 
						} catch {
							case (e:Exception) => {}
						}
						sents = List[String]();
					case _ => sent = sent + line
				}
			}
		}
		(success,total)
	}

	def process(args:Array[String]) = {
		DateTimeZone.setDefault(DateTimeZone.UTC);
		//--Arguments
		//(error check)
		if(args.length != 2){
			err("usage: gigaword [outdir] [file]")
			System.exit(1)
		}
		val outDir = new File(args(0))
		val inFile = new File(args(1))
		if(!outDir.isDirectory){ 
			throw new IllegalArgumentException("not a directory: " + outDir)
		}
		if(inFile.isDirectory){ 
			throw new IllegalArgumentException("input is a directory: " + inFile)
		}
		//--Process File
		val (success,total) = appendFile(outDir,inFile)

	}
}




//------------------------------------------------------------------------------
// TempEval2 Data Processor
//------------------------------------------------------------------------------
// -- RETOKENIZE --
abstract class TempEval2RetokTask extends Task[CoreMapDatum] {

	override def perform(d:Dataset[CoreMapDatum]):Unit = {
		val lang = this.language.toString
		log("RETOKENIZING " + lang)
		//(for each document)
		(0 until d.numExamples).foreach{ case (docIndex:Int) =>
			val sents=d.get(docIndex).get[JList[CoreMap],SentencesAnnotation](
				SENTENCES)
			//(check validity before)
			sents.foreach{ case (sent:CoreMap) =>
				DataProcessor.assertValidSentence(sent)
			}
			//(retokenize)
			DataLib.retokenize(d.get(docIndex))
			//(check validity after)
			sents.foreach{ case (sent:CoreMap) =>
				DataProcessor.assertValidSentence(sent)
			}
		}
		log("TAGGING " + lang)
		val lemma = new MorphaAnnotator(false)
		val pos = new POSTaggerAnnotator(
			System.getenv("HOME") +
				"/lib/data/bidirectional-distsim-wsj-0-18.tagger",
			false)
		(0 until d.numExamples).foreach{ case (docIndex:Int) =>
			//(process)
			val map:Annotation 
				= d.get(docIndex).implementation.asInstanceOf[Annotation]
			pos.annotate(map)
			lemma.annotate(map)
			//(check)
			val sents=map.get[java.util.List[CoreMap],SentencesAnnotation](SENTENCES)
			sents.foreach{ case (sent:CoreMap) =>
				DataProcessor.assertValidSentence(sent)
			}
		}
	}
	override def dependencies = Array[Class[_ <: Task[_]]]()
	override def name = "retok"
	def language:Language.Value
}

class TempEval2EnglishRetokTask extends TempEval2RetokTask
	{ override def language:Language.Value = Language.english }





// -- NUMBER NORMALIZE --
abstract class TempEval2NumberNormalizeTask extends Task[CoreMapDatum] {

	override def perform(d:Dataset[CoreMapDatum]):Unit = {
		val lang = this.language.toString
		log("NORMALIZING NUMBERS " + lang)
		//(for each document)
		(0 until d.numExamples).foreach{ case (docIndex:Int) =>
			DataLib.normalizeNumbers(d.get(docIndex))
			//(check validity)
			val sents=d.get(docIndex).get[java.util.List[CoreMap],
				SentencesAnnotation](SENTENCES)
			sents.foreach{ case (sent:CoreMap) =>
				DataProcessor.assertValidSentence(sent)
			}
		}
	}
	override def dependencies = Array[Class[_ <: Task[_]]]()
	override def name = "numbers"
	def language:Language.Value
}

class TempEval2EnglishNumberNormalizeTask extends TempEval2NumberNormalizeTask
	{ override def language:Language.Value = Language.english }







// -- INITIALIZE --
object TempEval2Task {
	lazy val pipeline:StanfordCoreNLP = {
		val props = new java.util.Properties
		props.setProperty("annotators","tokenize, ssplit, pos, lemma")
		props.setProperty("tokenize.whitespace", "true")
		props
		new StanfordCoreNLP(props)
	}
	lazy val tokenFact = 
		PTBTokenizer.factory(new CoreLabelTokenFactory(),
			PTBTokenizerAnnotator.DEFAULT_OPTIONS)

	def datasetName(lang:String):String = "aux/coremap/tempeval2-"+lang

	def tokenize(str:String,offsetZero:Int):Array[CoreLabel] = {
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

	def retok(args:Array[String]) = {
		//(overhead)
		if(args.length != 1){ 
			DataProcessor.exit("usage: tempeval2 retok [language]")
		}
		val lang = args(0) //TODO ignored for now
		log("Loading dataset...")
		val dataset = new SerializedCoreMapDataset(datasetName(lang))
		//(run task)
		dataset.runAndRegisterTask( new TempEval2EnglishRetokTask )
	}

	def numbers(args:Array[String]) = {
		if(args.length != 1){ 
			DataProcessor.exit("tempeval2 numbers takes no arguments")
		}
		val lang = args(0) //TODO ignored for now
		log("Loading dataset...")
		val dataset = new SerializedCoreMapDataset(datasetName(lang)+"-retok")
		//(run task)
		dataset.runAndRegisterTask( new TempEval2EnglishNumberNormalizeTask )
	}

	def init(args:Array[String]) = {
		//--Overhead
		log("INIT")
		//(error checking)
		if(args.length != 2){ 
			DataProcessor.exit("usage: tempeval2 init [directory] [language]")
		}
		//--Variables
		//(locations)
		val dir  = args(0)
		val lang = args(1)
		val train     = dir+"/training/"+lang+"/data"
		val test      = dir+"/test/"+lang+"/key"
		val baseTrain = train+"/base-segmentation.tab"
		val baseTest  = test+"/base-segmentation.tab"
		val dctTrain  = train+"/dct.txt"
		val dctTest   = dir+"/test/"+lang+"/dct-"+lang.substring(0,2)+".txt"
		val attrTrain = train+"/timex-attributes.tab"
		val attrTest  = test+"/timex-attributes.tab"
		val extTrain  = train+"/timex-extents.tab"
		val extTest   = test+"/timex-extents.tab"
		//(regexes)
		def mkline(n:Int) = {
			val b = new StringBuilder
			b.append("""\s*""")
			(0 until (n-1)).foreach{ (i:Int) => b.append("""([^\s]+)\s+""") }
			b.append("""([^\s]+)""")
			b.append("""\s*""")
			b.toString.r
		}
		val Dct = mkline(2)
		val Base = mkline(4)
		val Attr = mkline(8)
		val Ext = mkline(6)
		val PubTime = """([0-9]{4})([0-9]{2})([0-9]{2})""".r

		//--Util
		def eachLine(f:String, fn:String=>Any) =
			fromFile(new File(f)).getLines.foreach{ case (line:String) => fn(line) }

		//--Documents
		log("Reading Documents")
		//(vars)
		val docs = new HashMap[String,Annotation]
		var id = 0
		//(process)
		def processDct(line:String,test:Boolean) = {
			val Dct(name,pubTime) = line
			val PubTime(year,month,day) = pubTime
			val doc = new Annotation("")
			doc.set(classOf[CalendarAnnotation], 
				new DateTime(year.toInt,month.toInt,day.toInt,0,0,0,0)
				.toGregorianCalendar.asInstanceOf[Calendar])
			doc.set(classOf[IDAnnotation], ""+id)
			doc.set(classOf[DocIDAnnotation], name)
			doc.set(classOf[IsTestAnnotation], test)
			docs(name) = doc
			id += 1
		}
		//(read)
		eachLine(dctTrain, (line:String) => processDct(line,false) )
		eachLine(dctTest, (line:String) => processDct(line,true) )
		
		//--Words
		log("Reading Words")
		//(vars)
		val sent = new HashMap[String,StringBuilder]
		val verify = new HashMap[(String,Int),List[String]]
		//(process)
		def processBase(line:String) = {
			val Base(doc,sidText,offsetText,word) = line
			val sid = sidText.toInt
			if(!sent.contains(doc)){ sent(doc) = new StringBuilder }
			if(!verify.contains((doc,sid))){ verify((doc,sid)) = List[String]() }
			verify((doc,sid)) = word :: verify((doc,sid))
			sent(doc).append(word).append(" ")
		}
		//(read)
		eachLine(baseTrain, (line:String) => processBase(line) )
		eachLine(baseTest,  (line:String) => processBase(line) )
		//(digest)
		sent.foreach{ case (name:String,text:StringBuilder) =>
			docs(name).set(classOf[TextAnnotation], text.toString)
		}
		//(create annotation)
		docs.foreach{ case (name:String,doc:Annotation) =>
			//(variables)
			val gloss = sent(name).toString
			var sid = 0
			val sentences:java.util.List[CoreMap] = new java.util.ArrayList[CoreMap]
			//(process document)
			while(verify.contains((name,sid))){
				//(vars)
				val words = verify((name,sid)).reverse.toArray
				val sentMap = new ArrayCoreMap(1)
				val labels:java.util.List[CoreLabel]=new java.util.ArrayList[CoreLabel]
				sentMap.set(classOf[TokensAnnotation],labels)
				sentMap.set(classOf[TextAnnotation], words mkString " ")
				var offset = 0
				//(process sentence)
				words.foreach{ case (str:String) =>
					//(tokenize)
					val tokens = tokenize(str,offset).toList
					if(tokens.length > 0){
						val word = tokens.map{ _.word }.mkString(" ")
						val current = tokens.map{ _.originalText }.mkString("")
						tokens(0).setWord(word)
						tokens(0).setOriginalText(current)
						tokens(0).setEndPosition(tokens(tokens.length-1).endPosition)
					}
					labels.add(tokens(0))
					offset += str.length+1
				}
				//(add sentence)
				sid += 1
				DataProcessor.assertValidSentence(sentMap)
				sentences.add(sentMap)
			}
			doc.set(classOf[SentencesAnnotation],sentences)
		}
		
		//--Timexes Attributes
		log("Reading Timexes (attributes)")
		//(util)
		def getSentence(doc:String,sid:Int):CoreMap = {
			docs(doc)
				.get[java.util.List[CoreMap],SentencesAnnotation](SENTENCES)
				.get(sid)
		}
		def getTimex(sent:CoreMap,tid:String):CoreMap = {
			//(ensure timex list)
			if(!sent.containsKey[java.util.List[CoreMap],TimeExpressionsAnnotation](
					classOf[TimeExpressionsAnnotation])){
				sent.set(classOf[TimeExpressionsAnnotation],
					new ArrayList[CoreMap]()
						.asInstanceOf[java.util.List[CoreMap]])
			}
			//(find timex in list)
			val lst = sent.get[
				java.util.List[CoreMap],TimeExpressionsAnnotation](
				classOf[TimeExpressionsAnnotation])
			val filtered = lst.filter{ case (map:CoreMap) =>
				val cand = map.get[String,TimeIdentifierAnnotation](
					classOf[TimeIdentifierAnnotation])
				cand.equals(tid)
			}
			//(return timex)
			if(filtered.length == 1){
				filtered(0)
			} else if(filtered.length == 0){
				val timex = new ArrayCoreMap(4)
				timex.set(classOf[TimeIdentifierAnnotation], tid)
				lst.add(timex)
				timex
			} else{
				throw new IllegalStateException("Repeated timex in sentence!")
			}
		}
		def fastGetTimex(doc:String,sid:Int,tid:String):CoreMap = {
			getTimex(getSentence(doc,sid),tid)
		}
		//(process)
		def processAttr(line:String) = {
			//(variables)
			val Attr(doc,sidText,start,timex3,tid,junk,tag,value) = line
			val sent = getSentence(doc,sidText.toInt)
			val ground = new DateTime(docs(doc)
				.get[Calendar,CalendarAnnotation](classOf[CalendarAnnotation])
				.getTimeInMillis)
			val timex = getTimex(sent,tid)
			//(set annotations)
			timex.set(classOf[BeginIndexAnnotation], new java.lang.Integer(start))
			timex.set(classOf[OriginalTimeMetaAnnotation],
				new TimexMetaInfo(doc,sidText.toInt,start.toInt,tid))
			tag match {
				case "type" => timex.set(classOf[OriginalTimeTypeAnnotation], value)
				case "value" => 
					timex.set(classOf[OriginalTimeValueAnnotation], value)
					timex.set(classOf[TimeValueAnnotation], 
						DataLib.jodaTime2Array(DataLib.timex2JodaTime(value,ground),value))
			}
		}
		//(read)
		eachLine(attrTrain, (line:String) => processAttr(line) )
		eachLine(attrTest,  (line:String) => processAttr(line) )
		
		//--Timexes Extent
		log("Reading Timexes (extents)")
		//(vars)
		var countCond:(String,String,String) = null
		var lastWid:String = null
		//(process)
		def processCount(end:Int) = {
			//(update timex)
			val (d,s,t) = countCond
			val timex = fastGetTimex(d,s.toInt,t)
			timex.set(
				classOf[EndIndexAnnotation],new java.lang.Integer(end+1))
		}
		def processExt(line:String) = {
			//(variables)
			val Ext(doc,sidText,widText,timex3,tid,junk) = line
			if( !(doc,sidText,tid).equals(countCond) ){
				//(process)
				if(countCond != null){ processCount(lastWid.toInt) }
				//(reset cache)
				countCond = (doc,sidText,tid)
			}
			lastWid = widText
		}
		//(read)
		eachLine(extTrain, (line:String) => processExt(line) )
		eachLine(extTest,  (line:String) => processExt(line) )
		if(countCond != null){ processCount(lastWid.toInt) }

		//--Dump
		log("FLUSHING")
		val maps:Array[CoreMap] = docs.values.toList.sortBy{ case (map:CoreMap) =>
			map.get[String,IDAnnotation](classOf[IDAnnotation]).toInt
		}.toArray
		prettyLog("CoreMaps", maps)
		new SerializedCoreMapDataset(datasetName(lang),maps)
	}
}


case class DataProcessor(noop:Int)
object DataProcessor {
	def exit(msg:String) = {
		err("ERROR: " + msg)
		System.exit(1)
	}

	def assertValidSentence(sent:CoreMap) = {
		val tokens = sent.get[JList[CoreLabel],TokensAnnotation](TOKENS)
		val text:String = sent.get[String,TextAnnotation](TEXT)
		tokens.foreach{ (tok:CoreLabel) =>
			val begin = tok.beginPosition
			val end = tok.endPosition
			val word = tok.originalText
			val check = text.substring(begin,end)
			assert(word.equals(check), "Word '" + word + "' maps to '" + check + "'")
		}

	}

	def mkTempEval(args:Array[String]) = {
		log("ANNOTATING TempEval")
		if(args.length < 1){ exit("No task given (one of [init,retok,numbers] )") }
		args(0).toLowerCase match {
			case "init" => TempEval2Task.init(args.slice(1,args.length))
			case "retok" => TempEval2Task.retok(args.slice(1,args.length))
			case "numbers" => TempEval2Task.numbers(args.slice(1,args.length))
			case "all" => 
				TempEval2Task.init(args.slice(1,args.length))
				TempEval2Task.retok(Array[String](args(2)))
				TempEval2Task.numbers(Array[String](args(2)))
			case _ => exit("Unknown task: " + args(0))
		}
	}

	def mkAll(args:Array[String]) = {
		log("ANNOTATING Everything")
	}

	def main(args:Array[String]):Unit = {	
		val props = new Properties();
		props.setProperty("log.neatExit", "true");
		props.setProperty("log.console.trackStyle", "BOLD");
		props.setProperty("log.collapse", "approximate");
		StanfordRedwoodConfiguration.apply(props);

		DateTimeZone.setDefault(DateTimeZone.UTC);
		if(args.length < 1){ exit("No dataset given") }
		args(0).toLowerCase match {
			case "tempeval2" => TempEval2Dataset(args(1),args(2))
//			case "tempeval2" => mkTempEval(args.slice(1,args.length))
			case "gigaword" => Gigaword.process(args.slice(1,args.length))
			case _ => exit("Invalid dataset: " + args(0))
		}
	}

}



