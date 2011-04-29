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
	//TODO actually implement me
	def enterGuessGoldPos(guess:Range,gold:Array[Parse]):Int = 0
	def accuracy:Double = 1.0
	def avePos:Double = 0.0
	def aveScore(overconstrainingPenalty:Double, vaguenessPenalty:Double) = 1.0
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
		timexes.foreach( (t:Timex) => {
			val (parses,feedback) = fn(t.words)
			//TODO feedback
		})
		new Score
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
