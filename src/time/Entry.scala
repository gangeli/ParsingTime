package time

//(scala)
import scala.collection.mutable.HashMap
//(java)
import java.text.DecimalFormat
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.io.File
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import java.io.ObjectOutputStream
//(jodatime)
import org.joda.time.DateTimeZone
//(stanford)
import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.io.IOUtils
//(lib)
import org.goobs.exec.Execution
import org.goobs.util.Indexer
import org.goobs.util.MetaClass
import org.goobs.util.Stopwatch
import org.goobs.util.Def
import org.goobs.util.Static._
import org.goobs.stanford.SerializedCoreMapDataset
import org.goobs.stanford.StanfordExecutionLogInterface
import org.goobs.stats.CountStore
import org.goobs.nlp._



//------------------------------------------------------------------------------
// GLOBAL UTILITIES
//------------------------------------------------------------------------------
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
	val IsInt = """^(\-?[0-9]+)$""".r
	val CanInt = """^\-?[0-9]+(\.0+)?(E[0-9]+)?$""".r

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
			val w:Int = 
					if(O.ignoreCase) {
						G.wordIndexer.indexOf(str.toLowerCase)
					} else {
						G.wordIndexer.indexOf(str)
					}
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

	def isInt(str:String):Boolean = {
		str match {
			case G.IsInt(e) => true
			case G.CanInt(e,f) => true
			case _ => false
		}
	}
	def isInt(w:Int):Boolean = {
		isInt(w2str(w))
	}
	def str2int(str:String):Int = {
		str match {
			case G.IsInt(e) => str.toInt
			case G.CanInt(e,f) => str.toDouble.toInt
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
			var totalCnt:Double = 0.0
			override def getCount(key:Int):Double = counts(key)
			override def setCount(key:Int,count:Double):Unit = { 
				totalCnt += count - counts(key)
				counts(key) = count 
			}
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
			override def totalCount:Double = totalCnt
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
	case class Result(sent:TimeSent,guess:Temporal,gold:Temporal,exact:Boolean,
			ground:Time) {
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
	private var failedList = List[(TimeSent,Temporal,Time)]()

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
	def store(sent:TimeSent,guess:Temporal,gold:Temporal,exact:Boolean,
			ground:Time)={
		resultList = Result(sent,guess,gold,exact,ground) :: resultList
	}
	def logFailure(sent:TimeSent,gold:Temporal,ground:Time) = {
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

	def reportK:String = {
		"beam quality: " + exactRightK.map{ (count:Int) =>
			G.df.format(count.asInstanceOf[Double] / total.asInstanceOf[Double]) 
		}.mkString("  ")
	}
	def reportFailures(y:(String=>Any)):Unit = {
		failedList.foreach{ case (sent:TimeSent,gold:Temporal,ground:Time) =>
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
// DATA
//------------------------------------------------------------------------------
case class Data[T](train:DataStore[T],eval:DataStore[T]) {
	def noopLoop:Unit = {
		train.eachExample( -1 ).foreach{ x =>  }
		eval.eachExample( -1 ).foreach{ x =>  }
	}
}
trait DataStore[T] {
	def name:String
	def eachExample(i:Int):Iterable[T]
}
trait GroundingData extends DataStore[(TimeSent,Temporal,Time)]

class SimpleTimexStore(timexes:Array[Timex],test:Boolean,theName:String) 
		extends GroundingData {
	override def name:String = theName+{if(test){"-eval"}else{"-train"}}
	override def eachExample(iter:Int):Iterable[(TimeSent,Temporal,Time)] = {
		new Iterable[(TimeSent,Temporal,Time)] {
		override def iterator:Iterator[(TimeSent,Temporal,Time)] 
			= timexes.iterator.map{ (t:Timex) => 
				(TimeSent(t.tid,t.words(test),t.pos(test),t.nums), t.gold, t.grounding)
			}
		}
	}
}
object SimpleTimexStore {
	def apply(train:O.DataInfo,eval:O.DataInfo):Data[(TimeSent,Temporal,Time)] = {
		log("INPUT: /workspace/time/aux/coremap/tempeval2-english" +
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
				throw fail("Toy commented out!")
//				ToyData.STANDARD TODO
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

//------------------------------------------------------------------------------
// GROUNDING TASK
//------------------------------------------------------------------------------
trait TemporalTask {
	def run:Unit
}
class GroundingTask extends TemporalTask {
	//--Initialize JodaTime
	log("JodaTime settings")
	DateTimeZone.setDefault(DateTimeZone.UTC);
	//--Create Data
	//(dataset)
	forceTrack("loading dataset")
	//(timexes)
	val data = SimpleTimexStore(O.train,if(O.devTest) O.dev else O.test)
	endTrack("loading dataset")
	//--Create Parser
	startTrack("Creating Parser")
	assert(G.W > 0, "Words have not been interned yet!")
	Grammar.init(G.wordIndexer)
	var initialParser = CKYParser(G.W, Grammar.RULES)
	endTrack("Creating Parser")

	case class GoodOutput(tree:EvalTree[Any],value:Temporal,offset:Int)
	case class CompareElem(offset:Int,diff:(Duration,Duration),prob:Double)
	case class ScoreElem(index:Int,offset:Int,diff:(Duration,Duration),
			logProb:Double, temporal:Temporal){
		def exact:Boolean = { U.sumDiff(diff) <= O.exactMatchThreshold }
	}
	
	def compare(guess:Temporal, gold:Temporal,ground:GroundedRange
			):Iterator[CompareElem] = {
		import Lex._
		val INF = (Duration.INFINITE,Duration.INFINITE)
		def diff(gold:Temporal,guess:Temporal,second:Boolean):(Duration,Duration) 
				= (gold,guess) match {
			//--Immediate Invalids
			//(case: unks)
			case (gold:UnkTime,guess:Temporal) => INF
			case (gold:Temporal,guess:NoTime) => INF
			case (gold:Temporal,guess:Sequence) =>
				throw fail("Distribution returned a sequence: " + guess)
			case (gold:Sequence,guess:Temporal) =>
				throw fail("Gold is a sequence: " + gold)
			//(case: type errors)
			case (gold:FuzzyDuration,guess:GroundedDuration) => INF
			case (gold:GroundedDuration,guess:FuzzyDuration) => INF
			//--Valid
			//(case: durations)
			case (gold:FuzzyDuration,guess:FuzzyDuration) => 
				if(gold.largestUnit == guess.largestUnit){(Duration.ZERO,Duration.ZERO)}
				else{ INF }
			case (gold:GroundedDuration,guess:GroundedDuration) => 
				(guess-gold,Duration.ZERO)
			//(case: grounded ranges)
			case (gold:GroundedRange,guess:GroundedRange) => 
				if(guess.norm.seconds == 0 && gold.norm.seconds == 0){
					(Duration.ZERO,guess.begin-gold.begin) //case: instant
				} else if(O.instantAsDay && gold.norm.seconds == 0){
					(guess.begin-gold.begin,guess.end-(gold.end+DAY))
				} else {
					assert(!guess.begin.equals(Time.DAWN_OF) || guess.begin==Time.DAWN_OF)
					assert(!guess.end.equals(Time.END_OF) || guess.end==Time.END_OF)
					if(guess.begin == Time.DAWN_OF && gold.begin == Time.DAWN_OF){
						(Duration.ZERO,Duration.ZERO) //case: past
					} else if(guess.end == Time.END_OF && gold.end == Time.END_OF){
						(Duration.ZERO,Duration.ZERO) //case: future
					} else if(guess.begin == Time.DAWN_OF && gold.begin != Time.DAWN_OF){
						INF //case: beginning is neg_infinity
					} else if(guess.end == Time.END_OF && gold.end != Time.END_OF){
						INF //case: end is pos_infinity
					} else {
						(guess.begin-gold.begin,guess.end-gold.end) //case: can subtract
					}
				}
			//--Possibly Valid
			//(case: backoffs)
			case (gold:Range,guess:GroundedRange) => 
				if(second){ INF }
				else { val gr:GroundedRange = gold(ground); diff(gr,guess,true) }
			case (gold:PartialTime,guess:Temporal) =>
				if(second){ INF }
				else { val gr:GroundedRange = gold(ground); diff(gr,guess,true) }
			//--Not Valid
			//(case: didn't catch above)
			case (gold:Duration,guess:Duration) => throw fail("case: 2 durations")
			case (gold:Range,guess:Range) => throw fail("case: 2 durations")
			//(case: type error)
			case (gold:Duration,guess:Temporal) =>  INF
			case (gold:Range,guess:Temporal) => INF
			//(case: default fail)
			case _ => throw fail("Unk (invalid?) case: gold "+gold+" guess "+guess)
		}
		//--Map Iterator
		guess.distribution(ground).map{
				case (guess:Temporal,prob:Double,offset:Long) =>
			//(get diff)
			val d = diff(gold,guess,false)
			//(check timex consistency)
			if(U.sumDiff(d) > O.exactMatchThreshold){
				import edu.stanford.nlp.time.JodaTimeUtils._
				val (tGold, tGuess) = (gold,guess) match {
					case (a:UnkTime, b:Temporal) => {("not", "equal")}
					case (a:Temporal, b:NoTime) => {("not", "equal")}
					case (a:GroundedRange,b:GroundedRange) => {
						(timexDateValue(a.begin.base, a.end.base),
							timexDateValue(b.begin.base,b.end.base))
					}
					case (a:FuzzyDuration,b:FuzzyDuration) => {
						(timexDurationValue(a.interval.base,true),
							timexDurationValue(b.interval.base,true))
					}
					case (a:GroundedDuration,b:GroundedDuration) => {
						(timexDurationValue(a.interval.base),
								timexDurationValue(b.interval.base))
					}
					case _ => ("not","equal")
				}
				if(tGold.equals(tGuess)){
					err("Timexes match but " +
						"difference is nonzero: gold="+tGold+" guess="+tGuess+
						"  myGuess="+guess+"  inferredGold="+gold+" (diff="+d+") :: ")
				}
			}
			//(debug)
			assert(O.timeDistribution != O.Distribution.Point ||
				offset == 0L ||
				prob == 0.0,
				"Time returned distribution when it shouldn't have: " 
					+ guess + " (offset=" + offset + ") [prob=" + prob + "]")
			//(return)
			CompareElem(offset.toInt,d,prob)
		}
	}

	def handleParses(
			parses:Array[EvalTree[Any]],
			gold:Temporal,
			grounding:Time,
			score:Score,
			sent:TimeSent
			):Iterable[GoodOutput] = {
		//--Util
		val viterbi:Option[Temporal] = 
			if(parses != null && parses.length > 0) {
				val parse:Any = parses(0).evaluate
				parse match {
					case (time:Temporal) => Some(time(grounding))
					case _ => throw new IllegalStateException("Not a temporal: " + parse)
				}
			} else {
				None
			}
		log("Guess:  " + viterbi.orNull)
		log("Gold:   " + gold)
		log("Ground: " + grounding)
		//--Score Parses
		val scores:Array[ScoreElem]
			//(for each parse...)
			= parses.zipWithIndex.foldLeft((List[ScoreElem](),false)){ 
					case ((soFar:List[ScoreElem],isPruned:Boolean),
					      (parse:EvalTree[Any],i:Int)) => 
				if(!isPruned){
					//(variables)
					val ground:GroundedRange 
						= if(O.guessRange){ grounding.guessRange }
						  else{ Range(grounding,grounding) }
					val parseProb = parse.logProb
					//(timing)
					val parseWatch:Stopwatch = new Stopwatch
					parseWatch.start
					//(get guess temporal)
					val guess:Temporal = parse.evaluate match {
						case (t:Temporal) => t
						case _ => throw new IllegalStateException("Not a temporal: "+parse)
					}
					//(for each offset of parse...)
					val rtn = soFar ::: compare(guess,gold,ground).slice(0,O.scoreBeam)
						.map{ (elem:CompareElem) =>
							//(create parse)
							val resultLogProb = 
								if(O.includeTimeProb){ parse.logProb+math.log(elem.prob) }
								else{ parse.logProb }
							ScoreElem(i,elem.offset,elem.diff,resultLogProb,guess)
						}.toList
					//(timing & return)
					val lapTime = parseWatch.lap
					if(lapTime > O.pruneTime && i > O.pruneMinIndex){
						log("pruning after " + i)
						(rtn,true)
					} else {
						(rtn,false)
					}
				} else {
					(soFar,isPruned)
				}
		}._1.sortWith{ case (a:ScoreElem,b:ScoreElem) => 
			if(O.sortTimeProbInScore){
				//(case: order by P(parse)*P(time))
				if( (b.logProb - a.logProb).abs < 1e-6 ){
					if(a.index == b.index){
						b.offset.abs > a.offset.abs
					} else {
						b.index > a.index
					}
				} else {
					b.logProb < a.logProb 
				}
			} else {
				//(case: order by P(parse) breaking ties with P(time))
				if(a.index == b.index){
					b.logProb < a.logProb
				} else {
					b.index > a.index
				}
			}
		}.toArray
		log("" + scores.length + " candidates")
		//--Score Parses
		if(scores.length == 0){
			//(case: no scores)
			score.enter(false,(Duration.INFINITE,Duration.INFINITE), -1)
			score.store(sent,viterbi.orNull,gold,false,grounding)
			score.logFailure(sent,gold,grounding)
		} else {
			//(case: have score)
			//((get guess))
			val bestGuess = scores(0)
			assert(O.timeDistribution != O.Distribution.Point || 
				bestGuess.offset == 0,
				"Sanity check for time distribution")
			//((is in beam?))
			val correct:Array[ScoreElem] = scores.filter{ (elem:ScoreElem) => 
				assert(!elem.logProb.isNaN && elem.logProb <= 0.0, 
					"invalid probability")
				elem.exact }
			//((enter score))
			score.enter(bestGuess.exact,bestGuess.diff, 
				if(correct.length > 0) correct(0).index else -1)
			score.enterK(scores.slice(0,O.reportK).map{ _.exact })
			score.store(sent,viterbi.get,gold,bestGuess.exact,grounding)
			log("" + correct.length + " in beam")
		}
		//--Format Output
		//(case: scores)
		scores.map{ (elem:ScoreElem) =>
			if(elem.exact){
				Some(GoodOutput(parses(elem.index),elem.temporal,elem.offset))
			} else {
				None
			}
		}.filter{ _.isDefined }.map{ _.get }
	}

	override def run:Unit = {
		//--Run
		forceTrack("Running")
		//(train)
		val (parser,trainScores) =
				((O.iters-1) to 0 by -1).foldRight( (initialParser,List[Score]()) ){
					case (iter:Int,(parser:CKYParser,scores:List[Score])) =>
			forceTrack("Iteration " + iter)
			//(create score)
			val score = new Score
			//(iterate over timexes)
			val goodParses:Iterable[GoodOutput] 
					= data.train.eachExample(iter).zipWithIndex.flatMap{
						case ((sent:TimeSent,gold:Temporal,ground:Time),i:Int) =>
				startTrack("[" + i + "] " + sent.toString)
				//((parse))
				val parses:Array[EvalTree[Any]] = parser.parse(sent,O.beam)
				//((handle parses))
				val filteredParses = handleParses(parses, gold, ground, score, sent)
				//((continue map))
				endTrack("[" + i + "] " + sent.toString)
				filteredParses
			}
			log("finished parsing")
			//(update parser)
			val newParser = parser.update(goodParses.map{ _.tree })
			log("updated parser")
			//(update time)
			log("TODO update time distribution") //TODO
			//(continue loop)
			endTrack("Iteration " + iter)
			(newParser,scores)
		}
		endTrack("Running")
		//--Score
		startTrack(FORCE,BOLD,"Results")
//	reportScores(trainScores,testScore) //TODO
		endTrack("Results")
	}

	def reportScores(trainScores:Array[Score],testScore:Score) {
		val logger = Execution.getLogger();
		//--External Score
//		if(O.train.source == O.DataSource.English){ //TODO run me!
//			startTrack("TempEval")
//			//(variables)
//			Comparisons.inputDir = O.tempevalHome
//			Comparisons.outputDir = Execution.touch("")
//			Comparisons.lang = O.train.language
//			//(run)
//			val (trn,tst) = Comparisons.runSystem(sys=parser,quiet=true)
//			startTrack("Eval Results")
//			log(FORCE,BOLD,GREEN,"MyTime Train:     " + trn)
//			logger.setGlobalResult("train.tempeval.type", trn.typeAccuracy)
//			logger.setGlobalResult("train.tempeval.value", trn.valueAccuracy)
//			if(!O.devTest){
//				log(FORCE,BOLD,GREEN,"MyTime Test:      " + tst)
//				logger.setGlobalResult("test.tempeval.type", tst.typeAccuracy)
//				logger.setGlobalResult("test.tempeval.value", tst.valueAccuracy)
//			}
//			endTrack("Eval Results")
//			//(save info)
//			startTrack("Save Output")
//			val savData = parser.toInfo( Comparisons.dataset2inputs(
//				new TimeDataset(new SerializedCoreMapDataset(
//					System.getenv("HOME") + 
//						"/workspace/time/aux/coremap/tempeval2-english-retok-numbers"
//					)))
//			)
//			val outputFile = Execution.touch("tempeval2-output.ser")
//			IOUtils.writeObjectToFileNoExceptions(savData,outputFile.getPath)
//			endTrack("Save Output")
//			endTrack("TempEval")
//		}
		//--Process
		//(train)
		startTrack(BOLD,"train")
		logger.setGlobalResult("train.accuracy",
			trainScores(trainScores.length-1).accuracy)
		logger.setGlobalResult("train.averank",
			trainScores(trainScores.length-1).avePos)
		logger.setGlobalResult("train.inbeam",
			trainScores(trainScores.length-1).percentParsable)
		logger.setGlobalResult("train.score",
			trainScores(trainScores.length-1).aveScore())
		log(FORCE,BOLD,YELLOW,"train.accuracy: " + 
			trainScores(trainScores.length-1).accuracy)
		log(FORCE,YELLOW,"train.averank: " +	
			trainScores(trainScores.length-1).avePos)
		log(FORCE,YELLOW,"train.inbeam: " + 
			trainScores(trainScores.length-1).percentParsable)
		log(FORCE,YELLOW,"train.score: " + 
			trainScores(trainScores.length-1).aveScore())
		endTrack("train")
		//(test)
		val s = if(O.devTest) "dev" else "test"
		startTrack(BOLD,s)
		logger.setGlobalResult(s+".accuracy", testScore.accuracy)
		logger.setGlobalResult(s+".averank", testScore.avePos)
		logger.setGlobalResult(s+".inbeam", testScore.percentParsable)
		logger.setGlobalResult(s+".score", testScore.aveScore())
		log(FORCE,BOLD,YELLOW,s+".accuracy: "+ testScore.accuracy)
		log(FORCE,YELLOW,s+".averank: "+ testScore.avePos)
		log(FORCE,YELLOW,s+".inbeam: "+ testScore.percentParsable)
		log(FORCE,YELLOW,s+".score: "+ testScore.aveScore())
		endTrack(s)
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
	}
	
}

//------------------------------------------------------------------------------
// ENTRY
//------------------------------------------------------------------------------
class Entry {}
object Entry {
	import edu.stanford.nlp.util.logging._
	def main(args:Array[String]):Unit = {
		//--Exec
		Execution.exec(new Runnable(){
			override def run:Unit = {
				O.runDebug.toLowerCase match {
					//(case: time expression console)
					case "console" => Temporal.interactive
					//(case: don't run a debug sequence)
					case "none" => {
						(new GroundingTask).run
					}
					case _ => {
						throw fail("invalid runDebug flag")
					}
				}
			}
		}, args, new StanfordExecutionLogInterface)
	}
}

	
