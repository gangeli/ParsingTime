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
}

class Entry {
	private var times:Array[Datum] = null

	def init(corpus:String):Entry = {
		startTrack("Initializing")
		log("loading info file...")
		val info = new TimebankInfoFile(corpus)
		val files = info.getFiles
		//--Time Expressions
		startTrack("Timexes")
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
		//(get timexes)
		var timexes = List[(String,Timex)]()
		val iter = files.iterator
		while(iter.hasNext){
			val file:String = iter.next
			val titer = info.getTimexes(file).iterator
			while(titer.hasNext){
				timexes = (file, titer.next) :: timexes
			}
		}
		//(create time data)
		times = timexes.map( (pair:(String,Timex)) => {
			val (file,t) = pair
			val idStr:String = file + t.tid
			if(!t.text.trim.equals("")){ //filter text-less timexes
				val id:Int = index(idStr)
				G.idStringMap.put(id,idStr)
				val text = tokenize(t.text)
				new Datum(id, text, Time(t.value, t))
			} else {
				null
			}
		}).filter( (d:Datum) => d != null ).toArray
		//(sort)
		quickSort(times)
		times.foreach( (d:Datum) => {
			log(d);
		})
		endTrack
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
		Time.test
//		Execution.exec(new Runnable(){
//			override def run:Unit = {
//				(new Entry).init(O.timebank).train.test
//			}
//		}, args)
	}
}
