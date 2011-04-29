package time

import Lex._
import Conversions._
import ParseConversions._

import org.goobs.exec.Log._

//------------------------------------------------------------------------------
// PARSER
//------------------------------------------------------------------------------
object ParseConversions {
	implicit def range2parse(r:Range):Parse = Parse(r,null,null)
}
case class Parse(range:Range,duration:Duration,fn:Range=>Range)

trait Parser {
	def cycle(data:DataStore,iters:Int):Array[Score]
	def cycle(data:DataStore):Score = cycle(data,1)(0)
	def run(data:Data,iters:Int):(Array[Score],Score) = {
		start_track("Training")
		val train = cycle(data.train,iters)
		end_track
		start_track("Testing")
		val test = cycle(if(O.devTest) data.dev else data.test)
		end_track
		(train,test)
	}
}

class ItsAlwaysFriday extends Parser{
	override def cycle(data:DataStore,iters:Int):Array[Score] = {
		(1 to iters).map( (i:Int) => {
			start_track("Iteration " + i)
			val score = data.foreach( (sent:Array[Int]) => {
				val parse = FRI(NOW)
				(Array[Parse](FRI(NOW)), (place:Int,score:Double) => {})
			})
			log("Score: " + score)
			end_track
			score
		}).toArray
	}
}
