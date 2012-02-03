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
	lazy val W:Int = wordIndexer.size+1
	def P:Int = posIndexer.size
	lazy val UNK:Int = W-1
	val NUM:Int = wordIndexer.addAndGetIndex("--NUM--")
	def PUNK:Int = P
	def NUM(digits:Int,numType:NumberType.Value,test:Boolean) = {
		val suffix:String = numType match {
			case NumberType.NONE => "<<?>>"
			case NumberType.UNIT => "u"
			case NumberType.NUMBER => ""
			case NumberType.ORDINAL => "nd"
		}
		if(test){
			val w = wordIndexer.indexOf("--NUM("+digits+")"+suffix+"--")
			if(w < 0){ wordIndexer.indexOf("--NUM--") } else { w }
		} else {
			wordIndexer.addAndGetIndex("--NUM("+digits+")"+suffix+"--")
		}
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
	
	private val HasNum = """^(.*?)([0-9]+)(.*?)$""".r

	def getInt(str:String):Int = {
		str match {
			case HasNum(prefix,num,suffix) => num.toInt
		}
	}
	def w2str(w:Int):String = {
		if(w < G.wordIndexer.size) G.wordIndexer.get(w) else "--UNK--"
	}
	def str2w(str:String):Int = {
		str match {
			case HasNum(prefix,num,suffix) => G.NUM
			case _ => 
				if(O.ignoreCase) {
					G.wordIndexer.addAndGetIndex(str.toLowerCase)
				} else {
					G.wordIndexer.addAndGetIndex(str)
				}
		}
	}
	def str2wTest(str:String):Int = {
		assert(G.W == G.wordIndexer.size + 1)
		assert(G.UNK == G.wordIndexer.size)
		val w = str match {
			case HasNum(prefix,num,suffix) => G.NUM
			case _ => 
				if(O.ignoreCase) {
					G.wordIndexer.indexOf(str.toLowerCase)
				} else {
					G.wordIndexer.indexOf(str)
				}
			}
			assert(w < G.wordIndexer.size, "Invalid word returned")
			if(w < 0) G.UNK else w
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
