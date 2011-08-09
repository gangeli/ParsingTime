package time

//(scala)
import scala.util.Sorting._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.util.matching.Regex
//(java)
import java.io.StringReader
import java.text.DecimalFormat
//(jodatime)
import org.joda.time.DateTimeZone
//(lib)
import org.goobs.slib.Def
import org.goobs.testing._
import org.goobs.exec.Log._
import org.goobs.exec.Execution
import org.goobs.utils.Indexer;
import org.goobs.utils.MetaClass;
import org.goobs.utils.Stopwatch;
import org.goobs.testing.ResultLogger;



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
	def NUM(digits:Int) = wordIndexer.addAndGetIndex("--NUM("+digits+")--")

}

/**
	Global utility functions
*/
object U {
	def join[A](array:Array[A], str:String) = {
		if(array.length == 0){
			""
		} else {
			val sb:StringBuilder = new StringBuilder
			array.foreach( (a:A) => {
				sb.append(a).append(str)
			})
			sb.substring(0, sb.length - str.length)
		}
	}
	def accum[E](fn:(E=>Unit)=>Unit,aux:E=>Unit = (e:E)=>{}):List[E] = {
		var lst:List[E] = List[E]()
		fn( (e:E) => {
			aux(e)
			lst = e :: lst
		})
		lst.reverse
	}
	
	private val isNumTerm = """^--NUM(\([0-9]+\))?--$""".r
	def isNum(w:Int) = {
		w2str(w) match {
			case isNumTerm(e) => true
			case _ => false
		}
	}
	private def mkNum(str:String):Int = {
		if(O.bucketNumbers){
			G.NUM(str2int(str).toString.length)
		} else {
			G.NUM
		}
	}

	def w2str(w:Int):String = {
		if(w < G.W) G.wordIndexer.get(w) else "--UNK--"
	}
	def str2w(str:String):Int = {
		if(isInt(str)) mkNum(str) else G.wordIndexer.addAndGetIndex(str)
	}
	def str2wTest(str:String):Int = {
		if(isInt(str)){
			mkNum(str)
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
	def sent2str(sent:Array[Int]) = join(sent.map(w2str(_)), " ")

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

	def safeLn(d:Double) = {
		if(d == 0.0){ 
			Double.NegativeInfinity
		} else { 
			scala.math.log(d) 
		}
	}

	def rand:Double = G.random.nextDouble
	def randInt(begin:Int,end:Int):Int = G.random.nextInt(end-begin)+begin

	private val isInt = """^(\-?[0-9]+)$""".r
	private val canInt = """^(\-?[0-9]+\.0+)$""".r
	def isInt(str:String):Boolean = {
		str match {
			case isInt(e) => true
			case canInt(e) => true
			case _ => false
		}
	}
	def str2int(str:String):Int = {
		str match {
			case isInt(e) => str.toInt
			case canInt(e) => str.toDouble.toInt
			case _ => throw new IllegalArgumentException("Not an integer: " + str)
		}
	}

	def sigmoid(d:Double):Double = 1.0 / (1.0 + math.exp(-d))
			
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
	private var total:Int = 0
	private var sumPos = 0
	private var totalWithPos = 0
	private var goldMinusGuess = List[(Double,Double)]()
	private var resultList:List[Result] = List[Result]()

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
		if(position >= 0){
			sumPos += position
			totalWithPos += 1
		}
	}
	def store(sent:Sentence,guess:Temporal,gold:Temporal,exact:Boolean,t:Timex,
			ground:Time)={
		resultList = Result(sent,guess,gold,exact,t,ground) :: resultList
	}
	def accuracy:Double 
		= exactRight.asInstanceOf[Double]/total.asInstanceOf[Double]
	def avePos:Double 
		= sumPos.asInstanceOf[Double] / totalWithPos.asInstanceOf[Double]
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

	def tempeval(suffix:String):Unit = {
		import java.io.FileWriter
		//--Write Files
		val attrFile = new FileWriter(Execution.touch("attributes-"+suffix+".tab"))
		val extFile = new FileWriter(Execution.touch("extents-"+suffix+".tab"))
		results.sortBy( _.timex.tid ).foreach{ (r:Result) => 
			//(variables)
			val file:String = r.timex.sentence.document.filename
			val sent:Int = r.timex.sentence.indexInDocument
			val beginOffset:Int = r.timex.sentence.origIndex(r.timex.scopeBegin)
			val endOffset:Int = r.timex.sentence.origIndex(r.timex.scopeEnd)
			val timex3:String = "timex3"
			val tNum:String = "t"+(r.timex.indexInDocument+1)
			val one:String = "1"
			val typ:String = r.guess.timex3Type(r.ground)
			val value:String = if(typ.equals("UNK")){ "UNK" }
			                   else{ r.guess.timex3Value(r.ground) }
			
			def prefix(offset:Int):String
				= ""+file+"\t"+sent+"\t"+offset+"\t"+timex3+"\t"+tNum

			//(attributes)
			attrFile.write(prefix(beginOffset)+"\ttype\t"+typ+"\n")
			attrFile.write(prefix(beginOffset)+"\tvalue\t"+value+"\n")
			//(extents)
			(beginOffset until endOffset).foreach{ (offset:Int) =>
				extFile.write(prefix(offset)+"\t1\n")
			}
		}
		attrFile.close
		extFile.close
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
case class Data(train:DataStore,dev:DataStore,test:DataStore)

trait DataStore {
	def eachExample(fn:((Sentence,Int)=>(Array[Parse],Feedback=>Any)) ):Score
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
				diff:(Duration,Duration))
		def isExact(diff:(Duration,Duration)):Boolean
			= { U.sumDiff(diff) < O.exactMatchThreshold }
		val vitterbi:Temporal = 
			if(parses != null && parses.length > 0) {
				parses(0).ground(grounding) 
			} else {
				null
			}
		//--Score Parses
		val scores:Array[ScoreElem] 
			= parses.zipWithIndex.foldLeft(List[ScoreElem]()){ 
			case (soFar:List[ScoreElem],(parse:Parse,i:Int)) => 
				soFar ::: parse.scoreFrom(gold,grounding).slice(0,O.scoreBeam)
					.map{ case (diff:(Duration,Duration),score:Double,offset:Int) =>
						ScoreElem(i,offset,isExact(diff),diff)
					}.toList
		}.toArray
		//--Process Score
		if(scores.length > 0){
			//(get guess)
			val bestGuess = scores(0)
			//(is in beam?)
			val correct = scores.filter{ (elem:ScoreElem) => elem.exact }
			//(record)
			score.enter(bestGuess.exact,bestGuess.diff, 
				if(correct.length > 0) correct(0).index else -1)
			score.store(sent,vitterbi,gold,bestGuess.exact,timex,grounding)
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
		}
		vitterbi
	}
}

class SimpleTimexStore(timexes:Array[Timex]) extends DataStore{
	override def eachExample( 
			fn:((Sentence,Int)=>(Array[Parse],Feedback=>Any)) ):Score ={
		val score:Score = new Score
		//--Iterate
		//(timing variables)
		val watch:Stopwatch = new Stopwatch
		watch.start
		var parseTime:Double = 0.0
		var evalTime:Double = 0.0
		//(iterate over timexes)
		timexes.foreach{ (t:Timex) =>
			assert(t.words.length > 0, "Timex has no words: " + t)
			//(variables)
			val sent = Sentence(t.tid,t.words,t.pos,t.nums)
			//(parse)
			val (parses,feedback) = fn(sent, t.tid)
			parseTime += watch.lap
			//(score)
			val best:Temporal
				= handleParse(parses,t.gold,t.grounding,score,sent,feedback,t)
			evalTime += watch.lap
		}
		//--Return
		log("Timing: [parse] " + G.df.format(parseTime) +
			"  [eval] " + G.df.format(evalTime) + 
			" [ratio] " + G.df.format(parseTime/(parseTime+evalTime)) )
		score
	}
}

object SimpleTimexStore {
	def docs[S<:TimeSentence,E <: TimeDocument[S]](dataset:Dataset[E],
			begin:Int,end:Int,fn:E=>Unit):Unit = {
		val iter = dataset.slice(begin,end).iterator
		while(iter.hasNext){
			val doc = iter.next
			doc.init
			fn(doc)
		}
	}
	def timexes[S<:TimeSentence,E <: TimeDocument[S]](dataset:Dataset[E],
			begin:Int,end:Int,fn:Timex=>Unit,collectWords:Boolean):Unit = {
		docs[S,E](dataset,begin,end, (doc:E) => {
			val siter = doc.sentences.iterator
			while(siter.hasNext){
				val sent:S = siter.next
				sent.init(
					doc,
					if(collectWords) U.str2w(_) else U.str2wTest(_),
					if(collectWords) U.str2pos(_) else U.str2posTest(_)
					)
				val titer = sent.timexes.iterator
				while(titer.hasNext){
					fn(titer.next.ground(doc.grounding).setWords(sent))
				}
			}
		})
	}

	def apply[E <: TimeSentence,D<:TimeDocument[E]](dataset:Dataset[D]):Data = {
		//--Process Timexes
		start_track("Loading Timexes")
		logG("* train: " + O.train)
		logG({if(O.devTest) "*" else " "} + "   dev: " + O.dev)
		logG({if(!O.devTest) "*" else " "} + "  test: " + O.test)
		val data = Data(
			new SimpleTimexStore(
				U.accum(
					timexes[E,D](dataset,O.train.minInclusive,O.train.maxExclusive,
						_:Timex=>Unit,true), 
					(x:Timex) => log("[train] " + x) ).toArray),
			new SimpleTimexStore(
				U.accum(
					timexes[E,D](dataset,O.dev.minInclusive,O.dev.maxExclusive,
						_:Timex=>Unit,false), 
					(x:Timex) => log("[dev] " + x) ).toArray),
			new SimpleTimexStore(
				U.accum(
					timexes[E,D](dataset,O.test.minInclusive,O.test.maxExclusive,
						_:Timex=>Unit,false), 
					(x:Timex) => log("[test] " + x) ).toArray)
			)
		end_track
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

	private case class ToyStore(gold:Array[(String,Parse)]) extends DataStore {
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
				handleParse(parses,gold.value(todaysDate),todaysDate,score,s,feedback,
					null)
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
		Data(store(today).internWords,store(today),NONE)
	}
	
	def STANDARD:Data = {
		Data(
			store(
				today,
				week,aWeek,thisWeek,month,aMonth,ayear,thisYear,
				lastWeek,lastYear,lastQuarter,
				y1776,y17sp76,april1776,april2,weeks2,
				april
				).internWords,
//			store(week).internWords,
			store(lastMonth),
			NONE)
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
		start_track("Initializing")
		//--Initialize JodaTime
		log("JodaTime settings")
		DateTimeZone.setDefault(DateTimeZone.UTC);
		//--Load Data
		//(dataset)
		log("loading dataset")
		//(timexes)
		this.data = O.data match {
			case O.DataSource.Toy => 
				start_track("Toy Data")
				val data = ToyData.STANDARD
				end_track
				data
			case O.DataSource.Timebank =>
				SimpleTimexStore[TimebankSentence,TimebankDocument](
					Execution.getDataset(classOf[TimebankDocument]))
			case O.DataSource.English => 
				SimpleTimexStore[EnglishSentence,EnglishDocument](
					Execution.getDataset(classOf[EnglishDocument]))
			case _ => throw fail("Data source not implemented: " + this.data)
		}
		//--Create Parser
		start_track("Creating Parser")
		assert(G.W > 0, "Words have not been interned yet!")
		parser = new MetaClass("time."+O.parser).createInstance(classOf[Parser])
		end_track
		//--Return
		end_track
		this
	}


//------
// TRAIN/TEST
//------
	def run:Entry = {
		//--Run
		start_track("Running")
		val (trainScores:Array[Score],testScore:Score)
			= parser.run(this.data,O.iters)
		end_track
		//--Process
		start_track("Results")
		val logger = Execution.getLogger();
		//(train)
		start_track("train")
		logger.setGlobalResult("train.accuracy",
			trainScores(trainScores.length-1).accuracy)
		logger.setGlobalResult("train.averank",
			trainScores(trainScores.length-1).avePos)
		logger.setGlobalResult("train.inbeam",
			trainScores(trainScores.length-1).percentParsable)
		logger.setGlobalResult("train.score",
			trainScores(trainScores.length-1).aveScore())
		logG("train.accuracy: " + trainScores(trainScores.length-1).accuracy)
		logG("train.averank: " +	trainScores(trainScores.length-1).avePos)
		logG("train.inbeam: " + trainScores(trainScores.length-1).percentParsable)
		logG("train.score: " + trainScores(trainScores.length-1).aveScore())
		if(O.data != O.DataSource.Toy){
			trainScores(trainScores.length-1).tempeval("train")
		}
		end_track
		//(test)
		val s = if(O.devTest) "dev" else "test"
		start_track(s)
		logger.setGlobalResult(s+".accuracy", testScore.accuracy)
		logger.setGlobalResult(s+".averank", testScore.avePos)
		logger.setGlobalResult(s+".inbeam", testScore.percentParsable)
		logger.setGlobalResult(s+".score", testScore.aveScore())
		logG(s+".accuracy: "+ testScore.accuracy)
		logG(s+".averank: "+ testScore.avePos)
		logG(s+".inbeam: "+ testScore.percentParsable)
		logG(s+".score: "+ testScore.aveScore())
		if(O.data != O.DataSource.Toy){
			testScore.tempeval(s)
		}
		end_track
		end_track
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
		Execution.exec(new Runnable(){
			override def run:Unit = {
				O.runDebug.toLowerCase match {
					//(case: time expression console)
					case "console" => Temporal.interactive
					//(case: test the CRF)
					case "crf" => CKYParser.CRFTagger.debugSequence
					//(case: read the gold tag file)
					case "goldtagread" => Const.goldTag; logG("OK")
					//(case: don't run a debug sequence)
					case "none" => {
						(new Entry).init.run
					}
					case _ => {
						throw fail("invalid runDebug flag")
					}
				}
			}
		}, args)
	}
}

