package time

//(scala)
import scala.util.Sorting._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
//(java)
import java.io.StringReader
//(jodatime)
import org.joda.time.DateTimeZone
//(lib)
import org.goobs.slib.JMaps._
import org.goobs.testing._
import org.goobs.exec.Log._
import org.goobs.exec.Execution
import org.goobs.utils.Indexer;
import org.goobs.utils.MetaClass;

/**
	Globally accessible values
*/
object G {
	val wordIndexer = new Indexer[String]
	val idStringMap = new HashMap[Int,String]
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
}

//------------------------------------------------------------------------------
// AUXILLIARY CLASSES
//------------------------------------------------------------------------------
case class UNK()

class Score {
	private var exactRight:Int = 0
	private var total:Int = 0
	private var differences:List[Double] = List[Double]()
	def enter(exact:Boolean,diff:(Double,Double)) = {
		if(exact){ exactRight += 1 }
		total += 1
		differences = diff._1 :: differences
		differences = diff._2 :: differences
	}
	def accuracy:Double 
		= exactRight.asInstanceOf[Double]/total.asInstanceOf[Double]
	def avePos:Double = -1
	def aveScore(overconstrainingPenalty:Double=1, vaguenessPenalty:Double=1) = {
		//TODO
		1.0
	}
	override def toString:String = {
		"accuracy: "+accuracy+"; average pos: "+avePos+"; score: "+aveScore()
	}
}

//------------------------------------------------------------------------------
// DATA STORES
//------------------------------------------------------------------------------
case class Data(train:DataStore,dev:DataStore,test:DataStore)

trait DataStore {
	def foreach( fn:Array[Int]=>(Array[Parse],(Int,Double)=>Any) ):Score
}

class SimpleTimexStore(timexes:Array[Timex]) extends DataStore{
	override def foreach( 
			fn:Array[Int]=>(Array[Parse],(Int,Double)=>Any) ):Score = {
		val score:Score = new Score
		timexes.foreach( (t:Timex) => {
			val (parses,feedback) = fn(t.words)
			val gold = t.gold
			if(!gold.isInstanceOf[UNK]){
				//--Score Parses
				val scored:Array[(Int,Boolean,(Double,Double))] 
					= parses.zipWithIndex.map( (pair) => {
						val (parse,i) = pair
						//(score candidates)
						val diff:(Duration,Duration) 
							= if(gold.isInstanceOf[Range]){
								parse.rangeDiff(gold.asInstanceOf[Range], t.grounding)
							} else if(gold.isInstanceOf[Time]){
								parse.timeDiff(gold.asInstanceOf[Time], t.grounding)
							} else if(gold.isInstanceOf[Range=>Range]){
								parse.fnDiff(gold.asInstanceOf[Range=>Range], t.grounding)
							} else if(gold.isInstanceOf[Duration]){
								parse.durationDiff(gold.asInstanceOf[Duration], t.grounding)
							} else {
								throw fail("Cannot score timex " + t + " gold: " + gold)
							}
						//(accumulate output)
						val sumDiff:Double = 
								(  (diff._1.seconds+diff._2.seconds) 
									/ (60*60*24)  ).asInstanceOf[Double]
						val exactMatch:Boolean = sumDiff < 1.0
						def days(d:Duration) = (d.seconds / (60*60*24)).asInstanceOf[Double]
						(i,exactMatch,(days(diff._1),days(diff._2)))
					})
				//--Record Score
				score.enter(scored(0)._2,scored(0)._3)

				//TODO feedback
			} else {
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
						fn(titer.next.ground(doc.grounding))
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
// TRAIN
//------
	def run:Entry = {
		start_track("Running")
		val (trainScores:Array[Score],score:Score)
			= parser.run(this.timexData,O.iters)
		end_track
		this
	}
}

object Entry {
	def main(args:Array[String]):Unit = {
		Execution.exec(new Runnable(){
			override def run:Unit = {
				(new Entry).init.run
			}
		}, args)
	}
}
