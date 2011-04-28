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

case class UNK()

class Entry {
	private var dataset:Dataset[TimebankDocument] = null

	def init(corpus:String):Entry = {
		start_track("Initializing")
		//--Initialize JodaTime
		log("JodaTime settings")
		DateTimeZone.setDefault(DateTimeZone.UTC);
		//--Load Data
		log("loading dataset")
		dataset = Execution.getDataset(classOf[TimebankDocument])
		log("iterating over timexes")
		val iter = dataset.iterator
		while(iter.hasNext){
			val doc:TimebankDocument = iter.next
			val siter = doc.sentences.iterator
			while(siter.hasNext){
				val sent:TimebankSentence = siter.next
				println(sent)
				sent.refreshLinks
				val titer = sent.timexes.iterator
				while(titer.hasNext){
					val timex = titer.next
					println("  " + timex + " -> " + timex.gold)
				}
			}
		}
		end_track
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
