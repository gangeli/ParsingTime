package time

//(scala)
import scala.util.Sorting._;
//(javanlp)
import edu.stanford.nlp.ie.temporal.timebank._;
//(lib)
import org.goobs.slib.JMaps._
import org.goobs.exec.Execution
import org.goobs.exec.Log._

case class Datum(id:Int,text:String,value:Time) extends Ordered[Datum] {
	def compare(that:Datum) = this.id - that.id
}

class Entry {
	private var times:Array[Datum] = null

	def init(corpus:String):Entry = {
		val info = new TimebankInfoFile(corpus)
		val files = info.getFiles
		//--Time Expressions
		//(get timexes)
		var timexes = List[Timex]()
		val iter = files.iterator
		while(iter.hasNext){
			val file:String = iter.next
			val titer = info.getTimexes(file).iterator
			while(titer.hasNext){
				timexes = titer.next :: timexes
			}
		}
		//(create time data)
		times = timexes.map( (t:Timex) => {
			val tid:String = t.tid
			val id:Int = java.lang.Integer.parseInt(tid.substring(1,tid.length))
			new Datum(id, t.text, Time(t.value))
		}).toArray
		quickSort(times)

		times.foreach( (d:Datum) => {
			println(d);
		})
		//(return)
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
