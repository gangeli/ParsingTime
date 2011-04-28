package time

//(scala)
import scala.util.Sorting._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
//(java)
import java.io.StringReader
//(javanlp)
import edu.stanford.nlp.ie.temporal.timebank._
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.CoreLabelTokenFactory
//(lib)
import org.goobs.slib.JMaps._
import org.goobs.exec.Execution
import org.goobs.exec.Log._
import org.goobs.utils.Indexer;
//(misc)

case class Datum(id:Int,text:Array[Int],value:Time) extends Ordered[Datum] {
	override def compare(that:Datum) = this.id - that.id
	override def toString:String = {
		"Datum(" + id + "," + 
			U.join(text.map( (i:Int) => G.wordIndexer.get(i) )," ")  +
			" => " + value + ")"
	}
}

object G {
	val wordIndexer = new Indexer[String]
	val idStringMap = new HashMap[Int,String]
}

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
	def w2str(w:Int):String = G.wordIndexer.get(w)
}

class Entry {
	private var times:Array[Datum] = null

	def init(corpus:String):Entry = {
		startTrack("Initializing")
		log("loading info file...")
		val info = new TimebankInfoFile(corpus)
		val files = info.getFiles
		// -- FUNCTIONS --
		//(file iter)
		def eachFile[E](
				getter:String=>java.util.List[E],fn:(String,E)=>Unit) = {
			val iter = files.iterator
			while(iter.hasNext){
				val file:String = iter.next
				val titer = getter(file).iterator
				while(titer.hasNext){ fn(file, titer.next) }
			}
		}
		def accumFile[E](getter:String=>java.util.List[E]) = {
			var lst:List[(String,E)] = List[(String,E)]()
			eachFile(getter, (file:String,e:E) => lst = (file,e) :: lst)
			lst
		}
		//(ground timex)
		def ground(
				timexes:List[(String,Timex)],
				tlinks:List[(String,TLink)]
				):List[(String,Timex,List[TLink])] = {
			//(collect times)
			val hashTimes = new HashMap[(String,String),(Timex,List[TLink])]
			timexes.foreach( (pair:(String,Timex)) => {
				val (file,timex) = pair
				println("TIME: " + file + ": " + timex.tid)
				assert(!hashTimes.contains((file,timex.tid)), "duplicate timex")
				hashTimes.put((file,timex.tid),(timex,List[TLink]()))
			})
			//(link tlinks)
			tlinks.foreach( (pair:(String,TLink)) => {
				val (file,tlink) = pair
				println(tlink)
				val (startTime,startLst) = hashTimes((file,tlink.event1))
				hashTimes.put((file,tlink.event1), (startTime,tlink :: startLst))
				val (endTime,endLst) = hashTimes((file,tlink.event2))
				hashTimes.put((file,tlink.event2), (endTime,tlink :: endLst))
			})
			//(flatten)
			hashTimes.toList.map( (struct:((String,String),(Timex,List[TLink]))) => {
				val ((file,tid),(timex,links)) = struct
				(file,timex,links)
			})
		}
		//(id => index)
		val elemSet = new HashSet[String]
		def index(idStr:String):Int = {
			val size = elemSet.size
			elemSet += idStr
			assert(size < elemSet.size)
			elemSet.size
		}
		//(string => tokens)
		def tokenize(str:String):Array[Int] = {
			val tok = new PTBTokenizer(
					new StringReader(str), 
					new CoreLabelTokenFactory, 
					O.tokenizationOpts);
			var lst = List[Int]()
			while(tok.hasNext){
				lst = G.wordIndexer.addAndGetIndex(tok.next.word) :: lst
			}
			lst.reverse.toArray
		}
		// -- INIT --
		//--Time Expressions
		startTrack("Reading Info File")
		//(get timexes)
		log("Timexes")
		val timexes:List[(String,Timex)] 
			= accumFile((file:String) => { info.getTimexes(file) })
		//(get tlinks)
		log("TLinks")
		var tlinks:List[(String,TLink)] 
			= accumFile((file:String) => {info.getTlinksOfType(file,TLink.TIME_TIME)})
		//(ground links)
		log("Grounding links")
		val groundedLinks:List[(String,Timex,List[TLink])]
			= ground(timexes,tlinks)
		groundedLinks.foreach( (struct) => {
			val (file,timex,links) = struct
			println(timex.value + "  " + timex.text + " :: " + links)
		})
		endTrack
		
//		//(create time data)
//		times = timexes.map( (pair:(String,Timex)) => {
//			val (file,t) = pair
//			val idStr:String = file + t.tid
//			val id:Int = index(idStr)
//			G.idStringMap.put(id,idStr)
//			val text = tokenize(t.text)
//			val value = t.value
//			println(value + "  <-  " + U.join(text.map(U.w2str(_)), " "))
//			new Datum(id, text, null)
//		}).filter( (d:Datum) => d != null ).toArray
//		//(sort)
//		quickSort(times)
//		times.foreach( (d:Datum) => {
//			log(d);
//		})
		//(return)
		endTrack
		this
	}
	def train:Entry = {
		this
	}
	def test:Entry = {
		this
	}
}

object Entry {
	def main(args:Array[String]):Unit = {
		Execution.exec(new Runnable(){
			override def run:Unit = {
				(new Entry).init(O.timebank).train.test
			}
		}, args)
	}
}
