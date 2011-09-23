package time

//(scala)
import scala.collection.mutable.HashMap
//(java)
import java.text.DecimalFormat
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
//(jodatime)
import org.joda.time.DateTimeZone
//(stanford)
import edu.stanford.nlp.util.logging.Redwood.Static._
//(lib)
import org.goobs.slib.Def
import org.goobs.exec.Execution
import org.goobs.utils.Indexer
import org.goobs.utils.MetaClass
import org.goobs.utils.Stopwatch
import org.goobs.stanford.SerializedCoreMapDataset
import org.goobs.stanford.StanfordExecutionLogInterface
import org.goobs.stats.CountStore



/**
	Globally accessible values
*/
object G {
	val wordIndexer = new Indexer[String]
	val posIndexer = new Indexer[String]
	val idStringMap = new HashMap[Int,String]
	val df = new DecimalFormat("0.000")
	val pf = new DecimalFormat("0.0")
	val F_R = new Def[Range=>Range]
	val random:scala.util.Random =
		if(O.useSeed) new scala.util.Random(O.seed) else new scala.util.Random 
	def W:Int = wordIndexer.size
	def P:Int = posIndexer.size
	def UNK:Int = W
	def PUNK:Int = P
	val NUM:Int = wordIndexer.addAndGetIndex("--NUM--")
	def NUM(digits:Int,numType:NumberType.Value) = {
		val suffix:String = numType match {
			case NumberType.NONE => "<<?>>"
			case NumberType.UNIT => "u"
			case NumberType.NUMBER => ""
			case NumberType.ORDINAL => "nd"
		}
		wordIndexer.addAndGetIndex("--NUM("+digits+")"+suffix+"--")
	}

}

/**
	Global utility functions
*/
object U {
	def accum[E](fn:(E=>Unit)=>Unit,aux:E=>Unit = (e:E)=>{}):List[E] = {
		var lst:List[E] = List[E]()
		fn( (e:E) => {
			aux(e)
			lst = e :: lst
		})
		lst.reverse
	}
	
	private val IsNumTerm = """^--.*?(NUM).*?--$""".r
	private val HasNum = """^(.*?)([0-9]+)(.*?)$""".r
	def isNum(w:Int) = {
		w2str(w) match {
			case IsNumTerm(e) => true
			case _ => false
		}
	}
	private def mkNum(str:String,numType:NumberType.Value):Int = {
		assert(numType != NumberType.NONE, "NONE number type not allowed: "+str)
		if(O.bucketNumbers) {
			G.NUM(str2int(str).toString.length,numType)
		} else {
			G.NUM
		}
	}

	def w2str(w:Int):String = {
		if(w < G.W) G.wordIndexer.get(w) else "--UNK--"
	}
	def str2w(str:String):Int = str2w(str,NumberType.NONE)
	def str2w(str:String,numType:NumberType.Value):Int = {
		if(numType != NumberType.NONE) {
			val w = mkNum(str,numType) 
			assert(isNum(w), "Not recognized as number: " + w2str(w))
			w
		} else {
			str match {
				case HasNum(prefix,num,suffix) =>
					val w = G.wordIndexer.addAndGetIndex(
						"--["+prefix+"]NUM("+num.length+")["+suffix+"]--")
					assert(isNum(w), "Not recognized as number: " + w2str(w))
					w
				case _ => 
					if(O.ignoreCase) {
						G.wordIndexer.addAndGetIndex(str.toLowerCase)
					} else {
						G.wordIndexer.addAndGetIndex(str)
					}
			}
		}
	}
	def str2wTest(str:String):Int = str2wTest(str,NumberType.NONE)
	def str2wTest(str:String,numType:NumberType.Value):Int = {
		if(numType != NumberType.NONE) {
			mkNum(str,numType) 
		} else {
			val w:Int = G.wordIndexer.indexOf(str)
			if(w < 0) G.UNK else w
		}
	}
	def pos2str(pos:Int):String = 
		if(pos < G.P) G.posIndexer.get(pos) else "--UNK--"
	def str2pos(str:String):Int = G.posIndexer.addAndGetIndex(str)
	def str2posTest(str:String):Int = {
		val p:Int = G.posIndexer.indexOf(str)
		if(p < 0) G.PUNK else p
	}
	def sent2str(sent:Array[Int]) = sent.map(w2str(_)) mkString " "

	def sumDiff(diff:(Duration,Duration)):Int = {
		val secA:Long = diff._1.seconds.abs
		val secB:Long = diff._2.seconds.abs
		if(secA >= Int.MaxValue-secB){
			Int.MaxValue
		} else if(secB >= Int.MaxValue-secA){
			Int.MaxValue
		} else {
			assert( (secA+secB).intValue == secA+secB, "Integer overflow error" )
			(secA+secB).intValue
		}
	}

	def safeLn(d:Double):Double = {
		assert(!d.isNaN, "Taking the log of NaN")
		assert(d >= 0, "Taking the log of a negative number")
		if(d == 0.0){ 
			Double.NegativeInfinity
		} else { 
			scala.math.log(d) 
		}
	}
	def safeLn(d:Double,fallback:Double):Double = {
		if(d.isNaN){
			safeLn(fallback)
		} else {
			safeLn(d)
		}
	}

	def rand:Double = G.random.nextDouble
	def randInt(begin:Int,end:Int):Int = G.random.nextInt(end-begin)+begin

	private val IsInt = """^(\-?[0-9]+)$""".r
	private val CanInt = """^\-?[0-9]+(\.0+)?(E[0-9]+)?$""".r
	def isInt(str:String):Boolean = {
		str match {
			case IsInt(e) => true
			case CanInt(e,f) => true
			case _ => false
		}
	}
	def str2int(str:String):Int = {
		str match {
			case IsInt(e) => str.toInt
			case CanInt(e,f) => str.toDouble.toInt
			case _ => throw new IllegalArgumentException("Not an integer: " + str)
		}
	}

	def sigmoid(d:Double):Double = 1.0 / (1.0 + math.exp(-d))

	private lazy val badTimexes:Set[Int] = {
		import scala.io.Source.fromFile
		import java.io.File
		if(O.badTimexes == null || O.badTimexes.equalsIgnoreCase("false")){
			Set.empty
		} else {
			fromFile(new File(O.badTimexes)).getLines
				.filter{ (str:String) =>
					!str.trim.equals("") && !str.trim.startsWith("#")}
				.map{ (str:String) => str.toInt }.toArray.toSet
		}
	}
	def timexOK(tid:Int):Boolean = {
		!badTimexes.contains(tid)
	}

	def intStore(capacity:Int):CountStore[Int] = {
		val counts:Array[Double] = new Array[Double](capacity)
		new CountStore[Int] {
			override def getCount(key:Int):Double = counts(key)
			override def setCount(key:Int,count:Double):Unit = { counts(key) = count }
			override def emptyCopy:CountStore[Int] = intStore(capacity)
			override def clone:CountStore[Int] = {
				super.clone
				val copy = emptyCopy
				counts.zipWithIndex.foreach{ case (count:Double,i:Int) =>
					copy.setCount(i,count)
				}
				copy
			}
			override def clear:CountStore[Int] = {
				(0 until counts.length).foreach{ (i:Int) => counts(i) = 0 }
				this
			}
			override def iterator:java.util.Iterator[Int] = {
				var nextIndex = 0
				new java.util.Iterator[Int] {
					override def hasNext:Boolean = nextIndex < counts.length
					override def next:Int = {
						if(nextIndex >= counts.length){ throw new NoSuchElementException }
						nextIndex += 1
						nextIndex - 1
					}
					override def remove:Unit = {}
				}
			}
		}
	}
			
}

//------------------------------------------------------------------------------
// AUXILLIARY CLASSES
//------------------------------------------------------------------------------
case class UNK()

object Score {
	def duration2double(d:Duration)
		= d.seconds.asInstanceOf[Double] / O.scoreGranularity.asInstanceOf[Double]
	def score(
			diff:(Double,Double), 
			c_over:Double = O.c_overconstraining,
			c_vague:Double = O.c_vagueness ):Double = {
		val (start,end) = diff
		val cStart:Double = if(start < 0){ c_over } else { c_vague }
		val cEnd:Double = if(end < 0){ c_vague } else { c_over }
		val scoreStart:Double = (cStart / (cStart + start.abs))
		val scoreEnd:Double = (cEnd / (cEnd + end.abs))
		assert(scoreStart >= 0 && scoreStart <= 1.0, "Start score must be in range")
		assert(scoreEnd >= 0 && scoreEnd <= 1.0, "End score must be in range")
		if(scoreStart < 1e-10 && scoreEnd < 1e-10) {
			0.0 //score of zero
		} else {
			(scoreStart+scoreEnd)/2.0
		}
	}
	def score(diff:(Duration,Duration)):Double = {
		score( (duration2double(diff._1),duration2double(diff._2)) )
	}
}

class Score {
	case class Result(sent:Sentence,guess:Temporal,gold:Temporal,exact:Boolean,
			timex:Timex,ground:Time) {
		override def toString:String = {
			"" + {if(exact) "HIT  " else if(guess != null) "MISS " else "FAIL "} +
			sent + " as " + 
			{if(guess != null) guess else "<fail>"} + 
			" <gold " + gold + ">"
		}
	}
	private var exactRight:Int = 0
	private var exactRightK:Array[Int] = new Array[Int](O.reportK)
	private var total:Int = 0
	private var sumPos = 0
	private var totalWithPos = 0
	private var totalWithNonzeroPos = 0
	private var goldMinusGuess = List[(Double,Double)]()
	private var resultList:List[Result] = List[Result]()
	private var failedList = List[(Sentence,Temporal,Time)]()

	def releaseResults:Unit = { resultList = List[Result]() }
	def enter(exact:Boolean,diff:(Duration,Duration), position:Int) = {
		//(score exact)
		if(exact){ exactRight += 1 }
		total += 1
		//(store differences)
		goldMinusGuess
			= (Score.duration2double(diff._1), Score.duration2double(diff._2)) ::
				goldMinusGuess
		//(score position)
		if(position > 0){
			sumPos += position
			totalWithNonzeroPos += 1
		}
		if(position >= 0){
			totalWithPos += 1
		}
	}
	def enterK(topK:Array[Boolean]) = {
		assert(topK.length <= O.reportK, "Entering too many parses")
		(0 until exactRightK.length).foreach{ (i:Int) =>
			val anyOK:Boolean = !topK.slice(0,i+1).forall( !_ )
			if(anyOK){ exactRightK(i) += 1 }
		}
	}
	def store(sent:Sentence,guess:Temporal,gold:Temporal,exact:Boolean,t:Timex,
			ground:Time)={
		resultList = Result(sent,guess,gold,exact,t,ground) :: resultList
	}
	def logFailure(sent:Sentence,gold:Temporal,ground:Time) = {
		failedList = (sent,gold,ground) :: failedList
	}

	def accuracy:Double 
		= exactRight.asInstanceOf[Double]/total.asInstanceOf[Double]
	def avePos:Double 
		= sumPos.asInstanceOf[Double] / totalWithNonzeroPos.asInstanceOf[Double]
	def percentParsable:Double
		= (totalWithPos.asInstanceOf[Double] / total.asInstanceOf[Double])
	def aveScore(
			overconstrainingPenalty:Double = O.c_overconstraining, 
			vaguenessPenalty:Double = O.c_vagueness) = {
		val sum = goldMinusGuess.foldLeft(0.0)( 
			(soFar:Double,diff:(Double,Double)) => {
				val score = Score.score(diff,overconstrainingPenalty,vaguenessPenalty)
				assert(score == 0.0 || soFar+score > soFar, "Double overflow")
				soFar + score
			})
		sum / goldMinusGuess.length.asInstanceOf[Double]
	}
	def results:Array[Result] = resultList.reverse.toArray

	def tempeval(suffix:String):Unit = { //TODO
//		import java.io.FileWriter
//		//--Write Files
//		val attrFile = new FileWriter(Execution.touch("attributes-"+suffix+".tab"))
//		val extFile = new FileWriter(Execution.touch("extents-"+suffix+".tab"))
//		results.sortBy( _.timex.tid ).foreach{ (r:Result) => 
//			//(variables)
//			val file:String = r.timex.sentence.document.filename
//			val sent:Int = r.timex.sentence.indexInDocument
//			val beginOffset:Int = r.timex.goldSpan(0).toInt
//			val endOffset:Int = r.timex.goldSpan(1).toInt
//			val timex3:String = "timex3"
//			val tNum:String = r.timex.handle
//			val one:String = "1"
//			val typ:String = r.guess.timex3Type(Range(r.ground,r.ground))
//			val value:String = if(typ.equals("UNK")){ "UNK" }
//			                   else{ r.guess.timex3Value(Range(r.ground,r.ground)) }
//			
//			def prefix(offset:Int):String
//				= ""+file+"\t"+sent+"\t"+offset+"\t"+timex3+"\t"+tNum
//
//			//(attributes)
//			attrFile.write(prefix(beginOffset)+"\t1\ttype\t"+typ+"\n")
//			attrFile.write(prefix(beginOffset)+"\t1\tvalue\t"+value+"\n")
//			//(extents)
//			(beginOffset until endOffset).foreach{ (offset:Int) =>
//				extFile.write(prefix(offset)+"\t1\n")
//			}
//		}
//		attrFile.close
//		extFile.close
	}

	def reportK:String = {
		"beam quality: " + exactRightK.map{ (count:Int) =>
			G.df.format(count.asInstanceOf[Double] / total.asInstanceOf[Double]) 
		}.mkString("  ")
	}
	def reportFailures(y:(String=>Any)):Unit = {
		failedList.foreach{ case (sent:Sentence,gold:Temporal,ground:Time) =>
			y(sent.toString + " :: " + gold + " :: " + ground)
		}
	}

	override def toString:String = {
		"accuracy: "+G.df.format(accuracy)+"; average pos: "+G.df.format(avePos)+
			" (in " + G.pf.format((percentParsable*100)) + "%); score: "+
			G.df.format(aveScore())
	}
}

//------------------------------------------------------------------------------
// DATA STORES
//------------------------------------------------------------------------------
case class Data(train:DataStore,eval:DataStore){
	def noopLoop:Unit = {
		train.eachExample( (s,i) => (Array[Parse](),f=>{}) )
		eval.eachExample( (s,i) => (Array[Parse](),f=>{}) )
	}
}

trait DataStore {
	def eachExample(fn:((Sentence,Int)=>(Array[Parse],Feedback=>Any)) ):Score
	def name:String
	def handleParse(
			parses:Array[Parse], 
			gold:Temporal, 
			grounding:Time,
			score:Score,
			sent:Sentence,
			feedback:(Feedback=>Any),
			timex:Timex):Temporal = {
		//--Util
		case class ScoreElem(index:Int,offset:Int,exact:Boolean,
				diff:(Duration,Duration),prob:Double)
		def isExact(diff:(Duration,Duration)):Boolean
			= { U.sumDiff(diff) < O.exactMatchThreshold }
		val vitterbi:Temporal = 
			if(parses != null && parses.length > 0) {
				parses(0).ground(grounding) 
			} else {
				null
			}
		//--Score Parses
		startTrack("Evaluating")
//		log(FORCE,"ground: " + grounding)
		val scores:Array[ScoreElem] 
			= parses.zipWithIndex.foldLeft(List[ScoreElem]()){ 
			case (soFar:List[ScoreElem],(parse:Parse,i:Int)) => 
				val ground:GroundedRange 
					= if(O.guessRange){ grounding.guessRange }
					  else{ Range(grounding,grounding) }
//				log(FORCE,parse)
				soFar ::: parse.scoreFrom(gold,ground).slice(0,O.scoreBeam)
					.map{ case (diff:(Duration,Duration),prob:Double,offset:Int) =>
						ScoreElem(i,offset,isExact(diff),diff,prob)
					}.toList
		}.toArray
		endTrack("Evaluating")
		//--Process Score
		if(scores.length > 0){
			//(get guess)
			val bestGuess = scores(0)
			//(is in beam?)
			val correct = scores.filter{ (elem:ScoreElem) => 
				assert(elem.prob >= 0.0 && elem.prob <= 1.0, "invalid probability")
				elem.exact }
			//(record)
			score.enter(bestGuess.exact,bestGuess.diff, 
				if(correct.length > 0) correct(0).index else -1)
			score.enterK(scores.slice(0,O.reportK).map{ _.exact })
			score.store(sent,vitterbi,gold,bestGuess.exact,timex,grounding)
			if(correct.length == 0){ score.logFailure(sent,gold,grounding) }
			//(feedback)
			feedback(Feedback(
				gold, 
				grounding,
				correct.map( elem => (elem.index,elem.offset,Score.score(elem.diff)) ),
				scores.
					filter( elem => !elem.exact ).
					map( elem => (elem.index,elem.offset,Score.score(elem.diff)) )
				))
		} else {
			//(miss)
			score.enter(false,(Duration.INFINITE,Duration.INFINITE), -1)
			score.store(sent,vitterbi,gold,false,timex,grounding)
			score.logFailure(sent,gold,grounding)
		}
		vitterbi
	}
}

class SimpleTimexStore(timexes:Array[Timex],test:Boolean,theName:String) 
		extends DataStore{
	override def name:String = theName+{if(test){"-eval"}else{"-train"}}
	override def eachExample( 
			fn:((Sentence,Int)=>(Array[Parse],Feedback=>Any)) ):Score ={
		//(vars)
		val score:Score = new Score
		val shouldThread:Boolean = Execution.numThreads > 1
		//--Iterate
		//(timing variables)
		var parseTime:Double = 0.0
		var evalTime:Double = 0.0
		//(create runnables)
		startTrack("Creating Tasks")
		val tasks:Array[Runnable] = timexes
				.filter{ (t:Timex) => test || U.timexOK(t.tid) }
				.map{ (t:Timex) =>
			assert(t.words(test).length > 0, "Timex has no words: " + t)
			//(variables)
			val sent = Sentence(t.tid,t.words(test),t.pos(test),t.nums)
			new Runnable {
				override def run:Unit = {
					//(init)
					startTrack("Timex "+t.tid+"/"+timexes.length+": "+sent.toString)
					val watch:Stopwatch = new Stopwatch
					watch.start
					//(parse)
					val (parses,feedback) = fn(sent, t.tid)
					parseTime += watch.lap //not strictly threadsafe
					//(score)
					val best:Temporal
						= handleParse(parses,t.gold,t.grounding,score,sent,feedback,t)
					//(cleanup)
					evalTime += watch.lap //not stirctly threadsafe
					endTrack("Timex "+t.tid+"/"+timexes.length+": "+sent.toString)
					if(shouldThread){ finishThread }
				}
			}
		}
		endTrack("Creating Tasks")
		//(run runnables)
		if(shouldThread){
			val exec = Executors.newFixedThreadPool(Execution.numThreads)
			startThreads("Parsing")
			tasks.foreach{ (r:Runnable) => exec.submit(r) }
			exec.shutdown
			exec.awaitTermination(Long.MaxValue,TimeUnit.SECONDS)
			endThreads("Parsing")
		} else {
			tasks.foreach{ (r:Runnable) => r.run }
		}
		//--Return
		log("Timing: [parse] " + G.df.format(parseTime) +
			"  [eval] " + G.df.format(evalTime) + 
			" [parse/eval] " + G.df.format(parseTime/(parseTime+evalTime)) )
		score
	}
}

object SimpleTimexStore {
	def apply(train:O.DataInfo,eval:O.DataInfo):Data = {
		println("INPUT: /workspace/time/aux/coremap/tempeval2-english" +
							{if(O.retokenize) "-retok" else "" } +
							{if(O.collapseNumbers) "-numbers" else "" })
		def mkDataset(info:O.DataInfo):Array[Timex] = {
			info.source match {
				case O.DataSource.English => new TimeDataset(
					new SerializedCoreMapDataset(
						System.getenv("HOME") + 
							"/workspace/time/aux/coremap/tempeval2-english" +
							{if(O.retokenize) "-retok" else "" } +
							{if(O.collapseNumbers) "-numbers" else "" }
					).slice(info.begin,info.end)).timexes
				case O.DataSource.NYT => new TimeDataset(
					new SerializedCoreMapDataset(
						System.getenv("HOME") + 
							"/workspace/time/aux/processedNYT/"
					).slice(info.begin,info.end)).timexes
				case _ => throw fail("Unknown dataset")
			}
		}
		startTrack("Loading Timexes")
		//(log)
		log(FORCE,"train: " + train)
		log(FORCE,{if(O.devTest){ "dev:   " } else{ "test:  " }} + eval )
		//(make data)
		log("creating data")
		val data = 
			if(train.source == O.DataSource.Toy || eval.source == O.DataSource.Toy){
				ToyData.STANDARD
			} else {
				Data(
					new SimpleTimexStore(mkDataset(train),false,train.source.toString),
					new SimpleTimexStore(mkDataset(eval),true,eval.source.toString)
				)
			}
		//(loop over data to read everything)
		startTrack("NOOP loop")
		data.noopLoop
		endTrack("NOOP loop")
		//(return)
		endTrack("Loading Timexes")
		data
	}
}

object ToyData {
	import Lex._
	private val toys = new HashMap[String,Int]

	private val NONE = ToyStore(Array[(String,Parse)]())
	private def store(args:(String,Parse)*):ToyStore = ToyStore(args.toArray)
	private val today = ("today",Parse(TODAY))
	private val week = ("week",Parse(WEEK))
	private val aWeek = ("a week",Parse(WEEK))
	private val thisWeek = ("this week",Parse(REF ! WEEK))
	private val lastWeekToday = ("last week today",Parse(REF <<! WEEK))
	private val lastWeekNow = ("last week now",Parse(REF <<! WEEK))
	private val lastWeek = ("last week",Parse(REF <<! WEEK))
	private val pastWeek = ("past week",Parse(REF << WEEK))
	private val thePastWeek = ("the past week",Parse(REF << WEEK))
	private val pastMonths2 = ("past 2 months",Parse(REF << (MONTH*2)))
	private val weeks2 = ("2 week",Parse(WEEK*2))
	private val month = ("month",Parse(MONTH))
	private val aMonth = ("a month",Parse(MONTH))
	private val lastMonth = ("last month",Parse(REF <<! MONTH))
	private val lastQuarter = ("last quarter",Parse(REF <<! QUARTER))
	private val y1776 = ("1776",Parse(THEYEAR(1776)))
	private val y17sp76 = ("17 76",Parse(THEYEAR(1776)))
	private val months2 = ("2 months",Parse(MONTH*2))
	private val years2 = ("2 years",Parse(AYEAR*2))
	private val april = ("april",Parse(MOY(4)))
	private val april1776 = ("april 1776",Parse(MOY(4) ^ THEYEAR(1776)))
	private val april2 = ("april 2",Parse(MOY(4) ^ DOM(2)))
	private val ayear = ("a year",Parse(AYEAR))
	private val lastYear = ("last year",Parse(REF <<! AYEAR))
	private val thisYear = ("this year",Parse(REF ! AYEAR))
	private val monday = ("monday",Parse(DOW(1)(todaysDate,0)))
	private val friday_neg1 = ("friday",Parse(DOW(1)(todaysDate,-1)))

	private case class ToyStore(gold:Array[(String,Parse)]) extends DataStore {
		override def name:String = "toy"
		override def eachExample( 
				fn:((Sentence,Int)=>(Array[Parse],Feedback=>Any)) ):Score ={
			val score:Score = new Score
			gold.zipWithIndex.foreach{ case ((sent:String,gold:Parse),id:Int) =>
				//(variables)
				val words = sent.split(" ").map{ (str:String) => U.str2wTest(str) }
				val s = Sentence(
					-1,
					words, 
					words.map{ (w:Int) => U.str2posTest("UNK") },
					sent.split(" ").map{ (str:String) =>
						if(U.isInt(str)) U.str2int(str) else -1 }
					)
				if(!toys.contains(sent)){ toys(sent) = toys.size }
				//(parse)
				val (parses, feedback) = fn(s,toys(sent))
				//(feedback)
				handleParse(parses,
					gold.value(Range(todaysDate,todaysDate)),todaysDate,
					score,s,feedback,null)
			}
			score
		}
		def internWords:ToyStore = {
			gold.foreach{ case (sent:String,gold:Parse) =>
				sent.split(" ").foreach{ (str:String) => U.str2w(str) }
			}
			this
		}
	}

	def TODAY_ONLY:Data = {
		Data(store(today).internWords,store(today))
	}
	
	def STANDARD:Data = {
		Data(
			store(
			//--Train
				//(durations)
				week,aWeek,month,aMonth,ayear,weeks2,
				//(cannonicals)
				thisWeek,thisYear,
				//(shifts -- standard)
				lastWeek,lastYear,lastQuarter,
				//(shifts -- noncannonical)
				pastWeek,thePastWeek,pastMonths2,
				//(numbers -- basic)
				y1776,
//				//(numbers -- complex)
//				y17sp76,
				//(sequences)
				april,
				//(intersects)
				april1776,april2,
				//(ref)
				today
				).internWords,
			//--Test
			store(lastMonth))
	}
}

//------------------------------------------------------------------------------
// ENTRY
//------------------------------------------------------------------------------
class Entry {
	private var data:Data = null
	private var parser:Parser = null

//------
// INIT
//------
	def init:Entry = {
		startTrack("Initializing")
		//--Initialize JodaTime
		log("JodaTime settings")
		DateTimeZone.setDefault(DateTimeZone.UTC);
		//--Load Data
		//(dataset)
		forceTrack("loading dataset")
		//(timexes)
		this.data = SimpleTimexStore(O.train,if(O.devTest) O.dev else O.test)
		endTrack("loading dataset")
		//--Create Parser
		startTrack("Creating Parser")
		assert(G.W > 0, "Words have not been interned yet!")
		parser = new MetaClass("time."+O.parser).createInstance(classOf[Parser])
		endTrack("Creating Parser")
		//--Return
		endTrack("Initializing")
		this
	}


//------
// TRAIN/TEST
//------
	def run:Entry = {
		//--Run
		startTrack("Running")
		val (trainScores:Array[Score],testScore:Score)
			= parser.run(this.data,O.iters)
		endTrack("Running")
		//--Process
		startTrack("Results")
		val logger = Execution.getLogger();
		//(train)
		startTrack("train")
		logger.setGlobalResult("train.accuracy",
			trainScores(trainScores.length-1).accuracy)
		logger.setGlobalResult("train.averank",
			trainScores(trainScores.length-1).avePos)
		logger.setGlobalResult("train.inbeam",
			trainScores(trainScores.length-1).percentParsable)
		logger.setGlobalResult("train.score",
			trainScores(trainScores.length-1).aveScore())
		log(FORCE,"train.accuracy: " + trainScores(trainScores.length-1).accuracy)
		log(FORCE,"train.averank: " +	trainScores(trainScores.length-1).avePos)
		log(FORCE,"train.inbeam: " + trainScores(trainScores.length-1).percentParsable)
		log(FORCE,"train.score: " + trainScores(trainScores.length-1).aveScore())
		endTrack("train")
		//(test)
		val s = if(O.devTest) "dev" else "test"
		startTrack(s)
		logger.setGlobalResult(s+".accuracy", testScore.accuracy)
		logger.setGlobalResult(s+".averank", testScore.avePos)
		logger.setGlobalResult(s+".inbeam", testScore.percentParsable)
		logger.setGlobalResult(s+".score", testScore.aveScore())
		log(FORCE,s+".accuracy: "+ testScore.accuracy)
		log(FORCE,s+".averank: "+ testScore.avePos)
		log(FORCE,s+".inbeam: "+ testScore.percentParsable)
		log(FORCE,s+".score: "+ testScore.aveScore())
		endTrack(s)
		endTrack("Results")
		//--Debug dump
		log("saving parses")
		trainScores.zipWithIndex.foreach( pair => {
			val (score,i) = pair
			val writer = new java.io.FileWriter(Execution.touch("train"+i))
			score.results.foreach( r => {
				writer.write(r.toString); writer.write("\n")
			})
			writer.close
		})
		val writer = new java.io.FileWriter(Execution.touch(s))
		testScore.results.foreach( r => {
			writer.write(r.toString); writer.write("\n")
		})
		writer.close
		this
	}
}

object Entry {
	def main(args:Array[String]):Unit = {
		//--Exec
		Execution.exec(new Runnable(){
			override def run:Unit = {
				O.runDebug.toLowerCase match {
					//(case: time expression console)
					case "console" => Temporal.interactive
					//(case: test the CRF)
					case "crf" => CKYParser.CRFTagger.debugSequence
					//(case: read the gold tag file)
					case "goldtagread" => Const.goldTag; log(FORCE,"OK")
					//(case: don't run a debug sequence)
					case "none" => {
						(new Entry).init.run
					}
					case _ => {
						throw fail("invalid runDebug flag")
					}
				}
			}
		}, args, new StanfordExecutionLogInterface)
	}
}

	
