package time

//(scala)
import scala.util.Sorting._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
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
	def W:Int = wordIndexer.size
	def P:Int = posIndexer.size
	val F_R = new Def[Range=>Range]
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

	def w2str(w:Int):String = G.wordIndexer.get(w)
	def str2w(str:String):Int = G.wordIndexer.addAndGetIndex(str)
	def pos2str(pos:Int):String = G.posIndexer.get(pos)
	def pos2int(str:String):Int = G.posIndexer.addAndGetIndex(str)
	def sent2str(sent:Array[Int]) = join(sent.map(G.wordIndexer.get(_)), " ")

	def sumDiff(diff:(Duration,Duration)):Int = {
		val secA:Long = diff._1.seconds.abs
		val secB:Long = diff._2.seconds.abs
		if((secA+secB) > Integer.MAX_VALUE.longValue){
			java.lang.Integer.MAX_VALUE
		} else {
			(secA+secB).intValue
		}
	}

	def safeLn(d:Double) = {
		if(d == 0.0){ 
			java.lang.Double.NEGATIVE_INFINITY 
		} else { 
			scala.math.log(d) 
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
	case class Result(sent:Sentence,guess:Parse,gold:Any,exact:Boolean) {
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
	def store(sent:Sentence,guess:Parse,gold:Any,exact:Boolean) = {
		resultList = Result(sent,guess,gold,exact) :: resultList
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
	def eachExample(fn:Sentence=>(Array[Parse],Feedback=>Any) ):Score
}

class SimpleTimexStore(timexes:Array[Timex]) extends DataStore{
	override def eachExample( fn:Sentence=>(Array[Parse],Feedback=>Any) ):Score ={
		val score:Score = new Score
		timexes.foreach( (t:Timex) => {
			val (parses,feedback) = fn(Sentence(t.words,t.pos))
			val placeOneParse = 
				if(parses != null && parses.length > 0) parses(0) else null
			val gold = t.gold
			//--Score Parses
			val scored:Array[(Int,Boolean,(Duration,Duration))] 
				= parses.zipWithIndex.map( (pair) => {
					val (parse,i) = pair
					//(score candidates)
					val diff:(Duration,Duration) = gold match{
						case r:Range => {parse.rangeDiff(r, t.grounding)}
						case tm:Time => {parse.timeDiff(tm, t.grounding)}
						case (fn:(Range=>Range)) => {parse.fnDiff(fn, t.grounding)}
						case d:Duration => {parse.durationDiff(d, t.grounding)}
						case unk:UNK => {parse.unkDiff(unk)}
						case _:Any => {
							throw fail("Cannot score timex " + t + " gold: " + gold)
						}
					}
					//(accumulate output)
					val exactMatch:Boolean = U.sumDiff(diff) <= O.exactMatchThreshold
					(i,exactMatch,diff)
				})
			//--Get Best Index
			if(scored.length > 0){
				val (topIndex,topExact,topRange) = scored(0)
				//(sort)
				quickSort(scored)( Ordering.fromLessThan(
						( a:(Int,Boolean,(Duration,Duration)),
						  b:(Int,Boolean,(Duration,Duration))   ) => {
					val (aIndex,aExact,aDiff) = a
					val (bIndex,bExact,bDiff) = b
					val aSumSec:Int = U.sumDiff(aDiff)
					val bSumSec:Int = U.sumDiff(bDiff)
					if(aSumSec != bSumSec){
						aSumSec < bSumSec //order by difference
					} else {
						aIndex < bIndex //tiebreak by index
					}
				}))
				val (bestIndex,bestExact,bestRange) = scored(0)
				//--Record Score
				score.enter(topExact,topRange, if(bestExact) bestIndex else -1)
				score.store(Sentence(t.words,t.pos),placeOneParse,gold,topExact)
				//--Feedback
				feedback(Feedback(
					scored.
						filter( triple => triple._2 ).
						map( triple => (triple._1,Score.score(triple._3)) ),
					scored.
						filter( triple => !triple._2 ).
						map( triple => (triple._1,Score.score(triple._3)) )
					))
			} else {
				//--Record Miss
				score.enter(false,(Duration.INFINITE,Duration.INFINITE), -1)
				score.store(Sentence(t.words,t.pos),placeOneParse,gold,false)
			}
		})
		//--Return
		score
	}
}

//------------------------------------------------------------------------------
// ENTRY
//------------------------------------------------------------------------------
class Entry {
	private var dataset:Dataset[TimebankDocument] = null
	private var timexData:Data = null
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
		dataset = Execution.getDataset(classOf[TimebankDocument])
		def docs(begin:Int,end:Int,fn:TimebankDocument=>Unit):Unit = {
			val iter = dataset.slice(begin,end).iterator
			while(iter.hasNext){
				val doc = iter.next
				doc.init
				fn(doc)
			}
		}
		def timexes(begin:Int,end:Int,fn:Timex=>Unit):Unit = {
			docs(begin,end, (doc:TimebankDocument) => {
				val siter = doc.sentences.iterator
				while(siter.hasNext){
					val sent:TimebankSentence = siter.next
					sent.init
					val titer = sent.timexes.iterator
					while(titer.hasNext){
						fn(titer.next.ground(doc.grounding).setWords(sent))
					}
				}
			})
		}
		//(timexes)
		start_track("Loading Timexes")
		logG("* train: " + O.train)
		logG({if(O.devTest) "*" else " "} + "   dev: " + O.dev)
		logG({if(!O.devTest) "*" else " "} + "  test: " + O.test)
		this.timexData =  Data(
			new SimpleTimexStore(
				U.accum(
					timexes(O.train.minInclusive,O.train.maxExclusive,_:Timex=>Unit), 
					(x:Timex) => log("[train] " + x) ).toArray ),
			new SimpleTimexStore(
				U.accum(
					timexes(O.dev.minInclusive,O.dev.maxExclusive,_:Timex=>Unit), 
					(x:Timex) => log("[dev] " + x) ).toArray ),
			new SimpleTimexStore(
				U.accum(
					timexes(O.test.minInclusive,O.test.maxExclusive,_:Timex=>Unit), 
					(x:Timex) => log("[test] " + x) ).toArray )
			)
		end_track
		//--Create Parser
		start_track("Creating Parser")
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
			= parser.run(this.timexData,O.iters)
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
//		Time.interactive
//		SearchParser.interactive
		Execution.exec(new Runnable(){
			override def run:Unit = {
				val parser = new CKYParser
				(new Entry).init.run
			}
		}, args)
	}
}
