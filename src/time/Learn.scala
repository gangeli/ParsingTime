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
case class Parse(range:Range,duration:Duration,fn:Range=>Range){
	private def diff(a:Time, b:Time, ground:Time) = {
		val shouldGround = a.isGrounded || b.isGrounded
		val grndA = if(shouldGround && !a.isGrounded){ a(ground) } else { a }
		val grndB = if(shouldGround && !b.isGrounded){ b(ground) } else { b }
		grndB-grndA
	}
	def rangeDiff(gold:Range, guess:Range, ground:Time):(Duration,Duration) = {
		( diff(gold.begin, range.begin, ground),
			diff(gold.end, range.end, ground) )
	}
	def rangeDiff(gold:Range, ground:Time):(Duration,Duration) = {
		if(range != null){
			rangeDiff(gold, range, ground)
		} else if(fn != null){
			val grounding = Range(Time.DAWN_OF, Time.DAWN_OF)
			rangeDiff(gold, fn(grounding), ground)
		} else if(duration != null){
			(Duration.INFINITE, Duration.INFINITE)
		} else {
			throw fail("Parse is null")
		}
	}
	def timeDiff(gold:Time, ground:Time):(Duration,Duration) = {
		rangeDiff(Range(gold,gold), ground)
	}
	def fnDiff(gold:Range=>Range, ground:Time):(Duration,Duration) = {
		val grounding = Range(Time.DAWN_OF, Time.DAWN_OF)
		val groundedGold = gold(grounding)
		if(range != null){
			rangeDiff(groundedGold, range, ground)
		} else if(fn != null){
			rangeDiff(groundedGold, fn(grounding), ground)
		} else if(duration != null){
			(Duration.INFINITE, Duration.INFINITE)
		} else {
			throw fail("Parse is null")
		}
	}
	def durationDiff(gold:Duration, ground:Time):(Duration,Duration) = {
		if(duration != null){
			rangeDiff(Range(NOW, NOW+gold), Range(NOW, NOW+duration), ground)
		} else if(range != null){
			(Duration.INFINITE, Duration.INFINITE)
		} else if(fn != null){
			(Duration.INFINITE, Duration.INFINITE)
		} else {
			throw fail("Parse is null")
		}
	}
	override def toString:String = {
		if(range != null){ 
			range.toString
		} else if(duration != null){
			duration.toString
		} else if(fn != null){
			"<<function>>"
		} else {
			"<<no parse>>"
		}
	}
}

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
