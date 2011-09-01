package time

import java.util.Properties;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.{List => JList};
import java.io.File;
import java.io.FileInputStream;
import java.io.StringReader;

import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import scala.io.Source.fromFile

import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.ie.NumberNormalizer
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.pipeline.PTBTokenizerAnnotator
import edu.stanford.nlp.pipeline.MorphaAnnotator
import edu.stanford.nlp.pipeline.POSTaggerAnnotator
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.util._
import edu.stanford.nlp.time.{Timex => StanfordTimex}
import edu.stanford.nlp.time.GUTimeAnnotator
import edu.stanford.nlp.tagger.maxent.MaxentTagger
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.process.WhitespaceTokenizer

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
	val Tm = """^T([0-9]{2})([0-9]{2})$""".r
	val Year = """^([0-9]{2,4})$""".r
	val YearMonth = """^([0-9]{4})-?([0-9]{2})$""".r
	val YearMonthDay = """^([0-9]{4})-?([0-9]{2})-?([0-9]{2})$""".r
	val YearMonthDayHour = 
		"""^([0-9]{4})-?([0-9]{2})-?([0-9]{2})T?([0-9]{1,2})?$""".r
	val YearMonthDayTime = 
		"""^([0-9]{4})-?([0-9]{2})-?([0-9]{2})T?(MO|AF|EV|NI)?$""".r
	val YearWeekWE = """^([0-9]{4})-?W([0-9]{2})-?(WE)?$""".r
	val YearQuarter = """^([0-9]{4})-?Q([1-4])$""".r
	val YearHalf = """^([0-9]{4})-?H([1-2])$""".r
	val YearSeason = """^([0-9]{4})-?(SP|SU|FA|WI)$""".r
	val Period = """^P(([0-9]*|X)(D|W|M|Q|Y|E|C|L|H|S|T))+$""".r
	val periodPattern = java.util.regex.Pattern.compile(
		"""([0-9]*|X)(D|W|M|Q|Y|E|C|L|H|S|T)""")
	val Unk = """^(.*X.*)$""".r
	
	def timex2JodaTime(timex:String,ground:DateTime):Any = {
		val str = timex.trim
		val pass1 = str match {
			case Tm(hr,min) => 
				val base = ground.withHourOfDay(hr.toInt).withMinuteOfHour(min.toInt)
				(base, base.plusMinutes(1))
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
			case YearMonthDay(year,month,day) => 
				val base = new DateTime(year.toInt,month.toInt,day.toInt,0,0,0,0)
				(base, base.plusDays(1))
			case YearMonthDayHour(year,month,day,hour) => 
				val base = 
					new DateTime(year.toInt,month.toInt,day.toInt,hour.toInt-1,0,0,0)
				(base, base.plusHours(1))
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
				val (base,dur) = we match {
					case "WE" => 
						(new DateTime(year.toInt,1,1,0,0,0,0).
							withWeekOfWeekyear(week.toInt).withDayOfWeek(6), 2)
					case _ => (new DateTime(year.toInt,1,1,0,0,0,0).
							withWeekOfWeekyear(week.toInt), 7)
				}
				(base, base.plusDays(dur))
			case YearQuarter(year,quarter) =>
				val base = new DateTime(year.toInt,(quarter.toInt-1)*3+1,1,0,0,0,0)
				(base,base.plusMonths(3))
			case YearHalf(year,half) =>
				val base = new DateTime(year.toInt,(half.toInt-1)*6+1,1,0,0,0,0)
				(base,base.plusMonths(6))
			case YearSeason(year,season) =>
				val quarter = season match {
					case "WI" => 1
					case "SP" => 2
					case "SU" => 3
					case "FA" => 4
				}
				val base = new DateTime(year.toInt,(quarter.toInt-1)*3+1,1,0,0,0,0)
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
	
	def jodaTime2Array(time:Any):Array[String] = {
		time match {
			case (begin:DateTime,end:DateTime) =>
				Array[String]("RANGE",begin.toString,end.toString)
			case (begin:String,end:DateTime) =>
				Array[String]("RANGE",begin,end.toString)
			case (begin:DateTime,end:String) =>
				Array[String]("RANGE",begin.toString,end)
			case (begin:Period,fuzzy:Boolean) =>
				def mkVal(i:Int) = if(fuzzy) "x" else ""+i
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
				val endOffset = origTokens(oldEnd).endPosition
				timex.set(classOf[OriginalBeginIndexAnnotation],oldBegin)
				timex.set(classOf[OriginalEndIndexAnnotation],oldEnd)
				//(find new positions)
				retok.zipWithIndex.foreach{ case (tok:CoreLabel,index:Int) => 
					val candBegin = tok.beginPosition
					val candEnd = tok.endPosition
					if(candBegin == beginOffset){
						timex.set(classOf[BeginIndexAnnotation],
							new java.lang.Integer(index))
					}
					if(candEnd == endOffset){
						timex.set(classOf[EndIndexAnnotation],
							new java.lang.Integer(index+1))
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
}


////------------------------------------------------------------------------------
//// NUMBER ANNOTATOR
////------------------------------------------------------------------------------
//case class NumberAnnotator[S <: TimeSentence, T <: TimeTag](
//		sentClass:Class[S], tagClass:Class[T]) {
//	val did = 30452;
//	case class NAnn(start:Int,var len:Int,num:Number,t:String)
//
//	def run = {
//		//--Setup
//		//(database)
//		val db = Database.fromString(
//			"psql://research@localhost:data<what?why42?").connect
//		val sentences = db.getObjects(sentClass,
//			"SELECT * FROM "+Database.getTableName(sentClass))
//		//(javanlp pipeline)
//		val props = new Properties
//		props.setProperty("annotators","tokenize, ssplit, pos, lemma")
//		val pipeline = new StanfordCoreNLP(props)
//		//--Prepare
//		if(db.getFirstObjectWhere(classOf[Source],"did="+did) == null){
//			val source = db.emptyObject(classOf[Source])
//			source.did = did;
//			source.name = "Number Annotation for Time Data"
//			source.notes = ""
//			source.flush
//			println("CREATED SOURCE")
//		} else {
//			println("FOUND SOURCE")
//		}
//		val delCount = db.deleteObjectsWhere(tagClass, "did='"+did+"'")
//		println("DELETED " + delCount + " tags")
//		//--Modify
//		sentences.foreach{ (sent:S) => 
//			var numbers = List[NAnn]()
//			try{ //TODO get rid of me
//			//(annotate)
//			val (words,pos) = sent.bootstrap
//			println("-----")
//			val glossText = sent.gloss.replaceAll("/"," / ")
//				.replaceAll("""-"""," - ").replaceAll("""\s+"""," ")
//			println(glossText)
//			val input:Annotation = new Annotation(glossText)
//			pipeline.annotate(input)
//			val nums = NumberNormalizer.findAndMergeNumbers(input);
//			val tokens = input.get[JList[CoreLabel],TokensAnnotation](
//				classOf[TokensAnnotation])
//			if(tokens.size != words.length){
//				println(U.join(tokens.map{ x => x.word }.toArray,"_"))
//				throw new IllegalStateException("Length mismatch: " +
//					tokens.size + " versus " + words.length + " sent " + sent.sid);
//			}
//			//(collect numbers: vars)
//			var sentPointer = 0
//			var numsPointer = 0
//			var current:NAnn = null
//			//(collect numbers: algorithm)
//			while(sentPointer < words.length && numsPointer < nums.size) {
//				//(sync positions)
//				val term = nums(numsPointer)
//				val word = term.get[String,TextAnnotation](classOf[TextAnnotation])
//				//(get number)
//				val num = term.get[Number,NumericCompositeValueAnnotation](
//					classOf[NumericCompositeValueAnnotation])
//				var t = term.get[String,NumericCompositeTypeAnnotation](
//					classOf[NumericCompositeTypeAnnotation])
//				//(add any last annotation)
//				if(current != null){
//					while(!words(sentPointer).equals(word)){
//						sentPointer += 1
//						if(sentPointer >= sent.length){ 
//							throw new IllegalStateException(
//								"unbound on " + current + " waiting for '" + word + "'") 
//						}
//					}
//					current.len = sentPointer - current.start
//					numbers = current :: numbers
//					current = null
//				}
//				//(prepare new annotation)
//				if(num != null){
//					current = if(t == null) {
//							System.out.println("WARNING: NO TYPE FOR NUM: " + num) //TODO
//							NAnn(sentPointer,-1,num,"NUMBER") //TODO default
//						} else {
//							NAnn(sentPointer,-1,num,t)
//						}
//				}
//				sentPointer += 1
//				numsPointer += 1
//			}
//			//(add any dangling annotations)
//			if(current != null){
//				current.len = sentPointer - current.start
//				numbers = current :: numbers
//				current = null
//			}
//			} catch { //TODO get rid of me
//				case (e:Exception) => {
//					println("TODO: CAUGHT EXCEPTION " + e)
//					e.printStackTrace()
//				}
//			}
//			//(save to database)
//			numbers.foreach{ (num:NAnn) => 
//				//(value tag)
//				val start = db.emptyObject(tagClass)
//				start.wid = num.start+1
//				start.sid = sent.sid
//				start.did = did
//				start.key = "num"
//				start.value = num.num.toString
//				//(length tag)
//				val len = db.emptyObject(tagClass)
//				len.wid = num.start+1
//				len.sid = sent.sid
//				len.did = did
//				len.key = "num_length"
//				len.value = num.len.toString
//				//(type tag)
//				val numType = db.emptyObject(tagClass)
//				numType.wid = num.start+1
//				numType.sid = sent.sid
//				numType.did = did
//				numType.key = "num_type"
//				numType.value = num.t.toString
//				//(save)
//				start.flush
//				len.flush
//				numType.flush
//				println("  flushed " + num)
//			}
//		}
//	}
//}
//
//object NumberAnnotator {
//	def main(args:Array[String]) = {
//		if(args.length != 1){
//			System.err.println("usage: NumberAnnotator language")
//			System.exit(1)
//		}
//		args(0).toLowerCase match {
//		case "timebank" => 
//			(new NumberAnnotator(classOf[TimebankSentence],classOf[TimebankTag])).run
//		case "english" => 
//			(new NumberAnnotator(classOf[EnglishSentence],classOf[EnglishTag])).run
//		case _ =>
//			System.err.println("Unknown language: " + args(0))
//			System.exit(1)
//		}
//	}
//}


//------------------------------------------------------------------------------
// GUTime Data Processor
// TODO don't hard code tagger paths
//------------------------------------------------------------------------------

//object TimeAnnotator {
//	val db = Database.fromString(
//		"psql://research@localhost:data<what?why42?").connect
//	val did = 30453;
//	val date = """.*id\="[^0-9]+([0-9]{4})([0-9]{2})([0-9]{2})\.[0-9]+".*""".r
//	val name = """.*id\="([^"]+)".*""".r
//
//	val pipeline:AnnotationPipeline = {
//			val props = new Properties
//			props.setProperty("pos.model",
//				"/home/gabor/lib/data/bidirectional-distsim-wsj-0-18.tagger")
//			props.setProperty("annotators","tokenize, ssplit, pos")
//			val pipe = new StanfordCoreNLP(props);
////			pipe.addAnnotator(new PTBTokenizerAnnotator(false));
////			pipe.addAnnotator(new WordsToSentencesAnnotator(false));
////			pipe.addAnnotator(new POSTaggerAnnotator(false));
//			pipe
//		}
//
//	val tagger = new MaxentTagger(
//		"/home/gabor/lib/data/bidirectional-distsim-wsj-0-18.tagger")
//
//	def appendSentence(doc:GUTimeNYTDocument,sentOrigGloss:String
//			):(Int,Array[Int]) = {
//		val isSpecial = """^-(.*)-$""".r
//		val isDash = """^(-)$""".r
//		val isSlash = """^(\\?)/$""".r
//		//--Create Sentence
//		//(vars)
//		val sentOrigTokens = sentOrigGloss.split("""\s+""")
//		val sentGloss = U.join( sentOrigTokens.map{ (w:String) =>
//			w match {
//				case isSpecial(e) => w
//				case isDash(e) => w
//				case isSlash(e) => w
//				case _ => w.replaceAll("-"," - ").replaceAll("/"," / ")
//			} }, " ").replaceAll("""\s+"""," ")
//		val sent = sentGloss.split(" ")
//		//(create object)
//		val sentence = db.emptyObject(classOf[GUTimeNYTSentence])
//		sentence.fid = doc.fid
//		sentence.length = sent.length
//		sentence.gloss = sentGloss
//		sentence.flush
//		val sid = sentence.sid
//		//--Create Map
//		//(variables)
//		var offset = 0
//		val retok2orig:Array[Int] = sent.map{ x => 0 }
//		val orig2retok:Array[Int] =
//			(0 to sentOrigTokens.length).map{case (i:Int)=>
//				if(i == sentOrigTokens.length){
//					sent.length
//				} else {
//					val w = sentOrigTokens(i)
//					//(get jump)
//					val jump:Int = w match {
//							case isSpecial(e) => 0
//							case isDash(e) => 0
//							case isSlash(e) => 0
//							case _ =>
//								w.toCharArray.filter( (c:Char) => c == '-' || c == '/' ).length
//						}
//					//(create maps)
//					val rtn = offset
//					(offset until offset+1+jump*2).
//						foreach{ (fI:Int) => retok2orig(fI) = i }
//					offset += 1 + jump*2
//					rtn
//				}
//			}.toArray
//		//--Create Tags
//		//(helper function)
//		def mktag(wid:Int,key:String,value:String):GUTimeNYTTag = {
//			val tag = db.emptyObject(classOf[GUTimeNYTTag])
//			tag.sid = sid
//			tag.did = did
//			tag.wid = wid
//			tag.key = key
//			tag.value = value
//			tag
//		}
//		//(form and original)
//		sent.zipWithIndex.foreach{ case (w:String,i:Int) =>
//			mktag(i+1,"form",w).flush
//			mktag(i+1,"orig",""+retok2orig(i)).flush
//			7 //cast error otherwise?
//		}
//		//(tag)
//  	val tagged = tagger.tagSentence(sent)
//		tagged.zipWithIndex.foreach{ case (t,i) =>
//			mktag(i+1,"pos",t.tag).flush
//			7 //cast error otherwise?
//		}
//		//--Return
//		(sid,orig2retok)
//	}
//
//	def appendDoc(f:File, hdr:String, sentsGloss:Array[String]):Unit = {
//		//--Create Document
//		//(process header)
//		val date(year,month,day) = hdr
//		val name(filename) = hdr
//		val pubTime = new DateTime(year.toInt,month.toInt,day.toInt,0,0,0,0)
//		//(debug)
//		println("Processing " + filename + " {")
//		//(fill fields)
//		val doc:GUTimeNYTDocument = db.emptyObject(classOf[GUTimeNYTDocument])
//		doc.filename = filename
//		doc.pubTime = pubTime.toString
//		doc.notes="Automatically generated from GUTime"
//		//(flush)
//		doc.flush
//		println("  fid="+doc.fid)
//		//--Annotate
//		//(pre-annotate sentences)
//		val docGloss:String = 
//				sentsGloss.foldLeft(new StringBuilder){case (b:StringBuilder,l:String)=>
//					b.append(l).append("\n"); b }.toString 
//		val document = new Annotation(docGloss)
//		pipeline.annotate(document)
//		//(gutime annotation)
//		val calendar = pubTime.toGregorianCalendar.asInstanceOf[Calendar]
//		document.set(classOf[CalendarAnnotation], calendar)
//		val gutime = new GUTimeAnnotator(new File("etc/"));
//		gutime.annotate(document)
//		println("  (gutime annotated)")
//		//--Read Annotations
//		//(vars)
//		var offset:Int = 0
//		var sI:Int = 0
//		var wI:Int = 0
//		val sents:Array[Array[String]] = sentsGloss.map{ _.split(" ") }
//		//(save sentences)
//		val savedSents:Array[(Int,Array[Int])] 
//			= sentsGloss.map{ (sent:String) =>
//					appendSentence(doc,U.join( sent.toCharArray.map{ (c:Char) => 
//						if(c > 127) " " else Character.toString(c) },"") )
//				}
//		println("  (sentences annotated)")
//		//(iterate over timexes)
//		println("  timexes {")
//		var timexTag:Int = 1
//		document.get[
//			JList[CoreMap],TimexAnnotations](
//			classOf[TimexAnnotations]).foreach{ case (map:CoreMap) =>
//				//(get vars)
//				val begin:Int 
//					= map.get[java.lang.Integer,CharacterOffsetBeginAnnotation](
//						classOf[CharacterOffsetBeginAnnotation])
//				val end:Int 
//					= map.get[java.lang.Integer,CharacterOffsetEndAnnotation](
//						classOf[CharacterOffsetEndAnnotation])+1
//				val timex:StanfordTimex = map.get[StanfordTimex,TimexAnnotation](
//					classOf[TimexAnnotation])
//				//(find sentence and word)
//				//((find beginning)
//				while(sI < sents.length && (offset+sents(sI)(wI).length) <= begin){
//					offset += sents(sI)(wI).length + 1
//					wI += 1
//					if(wI >= sents(sI).length){
//						sI += 1; wI = 0;
//					}
//				}
//				var sentI = sI
//				val goldBegin = wI
//				val wBegin = savedSents(sentI)._2(goldBegin)
//				//((find end))
//				while(sI < sents.length && (offset+sents(sI)(wI).length) <= end){
//					offset += sents(sI)(wI).length + 1
//					wI += 1
//					if(wI >= sents(sI).length){
//						sI += 1; wI = 0;
//					}
//				}
//				//((fix end overflow))
//				val goldEnd = if(wI == 0){ sents(sentI).length } else { wI }
//				val wEnd = savedSents(sentI)._2(goldEnd)
//				//(process timex)
//				val timexValue = timex.value
//				val timexType = timex.timexType
//				if(timexValue != null){
//					val timex = db.emptyObject(classOf[GUTimeNYTTimex])
//					timex.sid = savedSents(sentI)._1
//					timex.scopeBegin = wBegin
//					timex.scopeEnd = wEnd
//					timex.timeType = timexType
//					val joda = DataLib.timex2JodaTime(timexValue,pubTime)
//					val arr  = DataLib.jodaTime2Array(joda)
//					timex.timeVal = arr
//					timex.originalValue = timexValue
//					timex.handle = "t"+timexTag; timexTag += 1
//					timex.goldSpan = Array[String](""+goldBegin,""+goldEnd)
//					timex.gloss = U.join(sents(sentI).slice(goldBegin,goldEnd)," ")
//					timex.flush
//					println("    "+timex)
//				}
//			}
//		println("  }")
//		println("}")
//	}
//	def appendFile(f:File):(Int,Int) = {
//		import scala.io.Source.fromFile
//		var lastDoc:String = ""
//		var inSent:Boolean = false
//		var sent:String = ""
//		var sents:List[String] = List[String]()
//		var success:Int = 0
//		var total:Int = 0
//		fromFile(f).getLines.foreach{ case (line:String) =>
//			if(line.startsWith("<DOC id")){
//				lastDoc = line
//			} else{
//				line.trim.toLowerCase match {
//					case "<s>" => assert(!inSent); inSent = true; sent = ""
//					case "</s>" => 
//						assert(inSent); inSent = false; 
//						sents = sent.replaceAll("""\s+"""," ") :: sents
//					case "<text>" => //noop
//					case "</text>" => //noop
//					case "</doc>" => 
//						try{
//							appendDoc(f,lastDoc,sents.reverse.toArray); 
//							success += 1
//						} catch {
//							case (e:Exception) => {}
//						}
//						total += 1
//						sents = List[String]();
//					case _ => sent = sent + line
//				}
//			}
//		}
//		(success,total)
//	}
//
//
//	def main(args:Array[String]) = {
//		DateTimeZone.setDefault(DateTimeZone.UTC);
//		//--Arguments
//		//(error check)
//		if(args.length != 1){
//			System.err.println("usage: TimeAnnotator files_directory")
//			System.exit(1)
//		}
//		//--Setup
//		//(add source)
//		if(db.getFirstObjectWhere(classOf[Source],"did="+did) == null){
//			val source = db.emptyObject(classOf[Source])
//			source.did = did;
//			source.name = "GUTime Automatically Generated Data"
//			source.notes = ""
//			source.flush
//			println("CREATED SOURCE")
//		} else {
//			println("FOUND SOURCE")
//		}
//		//(delete tables)
//		println("deleting GUTimeNYTDocument")
//		db.dropTable(classOf[GUTimeNYTDocument])
//		println("deleting GUTimeNYTSentence")
//		db.dropTable(classOf[GUTimeNYTSentence])
//		println("deleting GUTimeNYTTag")
//		db.dropTable(classOf[GUTimeNYTTag])
//		println("deleting GUTimeNYTTimex")
//		db.dropTable(classOf[GUTimeNYTTimex])
//		//--Process Files
//		val dir = new File(args(0))
//		assert(dir.isDirectory, "Input must be a directory")
//		var success:Int = 0
//		var total:Int = 0
//		db.beginTransaction
//		dir.listFiles.foreach{ case (f:File) => 
//			val (fileSuccess,fileTotal) = appendFile(f)
//			success += fileSuccess
//			total += fileTotal
//			println("FILE COMPLETE: " + success + " / " + total)
//		}
//		db.endTransaction
//		//--Create Indices
//		//--Print
//		println()
//		println("SUCCESSFULLY PROCESSED: " + success)
//		println("       TOTAL PROCESSED: " + total)
//	}
//}




//------------------------------------------------------------------------------
// TempEval2 Data Processor
//------------------------------------------------------------------------------
// -- RETOKENIZE --
abstract class TempEval2RetokTask extends Task[CoreMapDatum] {
	def retokSentence(sent:CoreMap):Unit = {
		//--Retokenize Sentence
		val origTokens=sent.get[java.util.List[CoreLabel],TokensAnnotation](TOKENS)
		val retok = origTokens.foldRight(List[CoreLabel]()){ 
				(word:CoreLabel,soFar:List[CoreLabel]) =>
			val orig = word.current
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
									TempEval2Task.tokenize("-",offset+tok.length).toList :::
									TempEval2Task.tokenize(tok.toString,offset
										).toList
								} else {
									toks ::: TempEval2Task.tokenize("-",offset).toList
								},
								offset+tok.length+1)
							//(tokenize on /)
							case '/' => (new StringBuilder,
								if(tok.length > 0){
									toks :::
									TempEval2Task.tokenize("/",offset+tok.length).toList :::
									TempEval2Task.tokenize(tok.toString,offset
										).toList
								} else {
									toks ::: TempEval2Task.tokenize("/",offset).toList
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

	override def perform(d:Dataset[CoreMapDatum]):Unit = {
		val lang = this.language.toString
		println("RETOKENIZING " + lang)
		//(for each document)
		(0 until d.numExamples).foreach{ case (docIndex:Int) =>
			val map:CoreMap = d.get(docIndex)
			val sents=map.get[java.util.List[CoreMap],SentencesAnnotation](SENTENCES)
			//(for each sentence)
			sents.foreach{ case (sent:CoreMap) =>
				//(retokenize sentence)
				DataProcessor.assertValidSentence(sent)
				retokSentence(sent)
				DataProcessor.assertValidSentence(sent)
			}
		}
		println("TAGGING " + lang)
		val lemma = new MorphaAnnotator(false)
		val pos = new POSTaggerAnnotator(false)
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
	def findNumbers(sent:CoreMap) = {
		//(set numbers)
		val nums:JList[CoreMap] = NumberNormalizer.findAndMergeNumbers(sent);
		val tokens:JList[CoreLabel] = nums.map{ new CoreLabel(_) }.toList
		sent.set(classOf[TokensAnnotation], tokens )
		//(relink timexes)
		DataLib.relinkTimexes(sent)
		//(set current annotation)
		tokens.foreach{ (num:CoreLabel) =>
			if(num.current == null || num.current.equals("")){
				num.setCurrent(num.word)
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

	override def perform(d:Dataset[CoreMapDatum]):Unit = {
		val lang = this.language.toString
		println("NORMALIZING NUMBERS " + lang)
		//(for each document)
		(0 until d.numExamples).foreach{ case (docIndex:Int) =>
			val map:CoreMap = d.get(docIndex)
			val sents=map.get[java.util.List[CoreMap],SentencesAnnotation](SENTENCES)
			//(for each sentence)
			sents.foreach{ case (sent:CoreMap) =>
				//(normalize numbers)
				DataProcessor.assertValidSentence(sent)
				findNumbers(sent)
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
				new java.lang.Integer(offset+label.current.length))
				offset += label.current.length
			label
		}.toArray
	}

	def retok(args:Array[String]) = {
		//(overhead)
		if(args.length != 1){ 
			DataProcessor.exit("usage: tempeval2 retok [language]")
		}
		val lang = args(0) //TODO ignored for now
		println("Loading dataset...")
		val dataset = new SerializedCoreMapDataset(datasetName(lang))
		//(run task)
		dataset.runAndRegisterTask( new TempEval2EnglishRetokTask )
	}

	def numbers(args:Array[String]) = {
		if(args.length != 1){ 
			DataProcessor.exit("tempeval2 numbers takes no arguments")
		}
		val lang = args(0) //TODO ignored for now
		println("Loading dataset...")
		val dataset = new SerializedCoreMapDataset(datasetName(lang)+"-retok")
		//(run task)
		dataset.runAndRegisterTask( new TempEval2EnglishNumberNormalizeTask )
	}

	def init(args:Array[String]) = {
		//--Overhead
		println("INIT")
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
		println("Reading Documents")
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
		println("Reading Words")
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
						val current = tokens.map{ _.current }.mkString("")
						tokens(0).setWord(word)
						tokens(0).setCurrent(current)
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
		println("Reading Timexes (attributes)")
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
					new java.util.ArrayList[CoreMap]()
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
			tag match {
				case "type" => timex.set(classOf[OriginalTimeTypeAnnotation], value)
				case "value" => 
					timex.set(classOf[OriginalTimeValueAnnotation], value)
					timex.set(classOf[TimeValueAnnotation], 
						DataLib.jodaTime2Array(DataLib.timex2JodaTime(value,ground)))
			}
		}
		//(read)
		eachLine(attrTrain, (line:String) => processAttr(line) )
		eachLine(attrTest,  (line:String) => processAttr(line) )
		
		//--Timexes Extent
		println("Reading Timexes (extents)")
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
		println("FLUSHING")
		val maps:Array[CoreMap] = docs.values.toList.sortBy{ case (map:CoreMap) =>
			map.get[String,IDAnnotation](classOf[IDAnnotation]).toInt
		}.toArray
		new SerializedCoreMapDataset(datasetName(lang),maps)
	}
}


case class DataProcessor(noop:Int)
object DataProcessor {
	val db = Database.fromString(
		"psql://research@localhost:data<what?why42?").connect
	def exit(msg:String) = {
		println("ERROR: " + msg)
		System.exit(1)
	}

	def assertValidSentence(sent:CoreMap) = {
		val tokens = sent.get[JList[CoreLabel],TokensAnnotation](TOKENS)
		val text:String = sent.get[String,TextAnnotation](TEXT)
		tokens.foreach{ (tok:CoreLabel) =>
			val begin = tok.beginPosition
			val end = tok.endPosition
			val word = tok.current
			val check = text.substring(begin,end)
			assert(word.equals(check), "Word '" + word + "' maps to '" + check + "'")
		}

	}

	def mkTempEval(args:Array[String]) = {
		println("ANNOTATING TempEval")
		if(args.length < 1){ exit("No task given (one of [init,retok,numbers] )") }
		args(0).toLowerCase match {
			case "init" => TempEval2Task.init(args.slice(1,args.length))
			case "retok" => TempEval2Task.retok(args.slice(1,args.length))
			case "numbers" => TempEval2Task.numbers(args.slice(1,args.length))
			case _ => exit("Unknown task: " + args(0))
		}
	}

	def main(args:Array[String]):Unit = {	
		DateTimeZone.setDefault(DateTimeZone.UTC);
		if(args.length < 1){ exit("No dataset given") }
		args(0).toLowerCase match {
			case "tempeval2" => mkTempEval(args.slice(1,args.length))
			case _ => exit("Invalid dataset: " + args(0))
		}
	}


}



