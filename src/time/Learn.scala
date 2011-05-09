package time

import Lex._
import Conversions._
import ParseConversions._

import org.goobs.exec.Log._

import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.stats.Counters;

//------------------------------------------------------------------------------
// PARSER
//------------------------------------------------------------------------------
object ParseConversions {
	implicit def time2parse(t:Time):Parse = Parse(Range(t,t),null,null)
	implicit def range2parse(r:Range):Parse = Parse(r,null,null)
	implicit def duration2parse(d:Duration):Parse = Parse(null,d,null)
	implicit def fn2parse(fn:Range=>Range):Parse = Parse(null,null,fn)
}

case class Sentence(words:Array[Int],pos:Array[Int]) {
	def apply(i:Int) = words(i)
	def foreach(fn:(Int,Int)=>Any) = words.zip(pos).foreach(Function.tupled(fn))
}

case class Parse(range:Range,duration:Duration,fn:Range=>Range){
	private def diff(a:Time, b:Time, ground:Time) = {
		val shouldGround = !a.isGrounded || !b.isGrounded
		val grndA = if(shouldGround && !a.isGrounded){ a(ground) } else { a }
		val grndB = if(shouldGround && !b.isGrounded){ b(ground) } else { b }
		grndB-grndA
	}
	def unkDiff(gold:UNK):(Duration,Duration) = {
		(Duration.INFINITE, Duration.INFINITE)
	}
	def rangeDiff(gold:Range, guess:Range, ground:Time):(Duration,Duration) = {
		assert(gold != null, "gold is null")
		assert(guess != null, "guess is null")
		( diff(gold.begin, guess.begin, ground),
			diff(gold.end, guess.end, ground) )
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
		val grounding = Range(Time.DAWN_OF, Time.END_OF)
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
			rangeDiff(Range(NOW, NOW+gold.flatten), 
				Range(NOW, NOW+duration.flatten), ground)
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

trait StandardParser extends Parser {
	def parse(iter:Int, sent:Sentence):(Array[Parse],(Int,Boolean,Double)=>Any)
	override def cycle(data:DataStore,iters:Int):Array[Score] = {
		(1 to iters).map( (i:Int) => {
			start_track("Iteration " + i)
			val score = data.eachExample( (sent:Sentence) => {
				parse(i, sent)
			})
			log("Score: " + score)
			end_track
			score
		}).toArray
	}
}

class ItsAlwaysFriday extends StandardParser{
	override def parse(i:Int, sent:Sentence
			):(Array[Parse],(Int,Boolean,Double)=>Any)={
		val parse:Array[Parse] = Array[Parse](
			FRI(NOW),                              // I think it's friday
			(r:Range) => Range(r.begin,NOW),       // or 'the past'
			WEEK,                                  // or a week
			Range(Time(2011,4,26),Time(2011,4,27)) // or April 26
			)
		(parse, (index:Int,exact:Boolean,score:Double) => {})
	}
}



class PrimitivesOnly extends StandardParser{
	val simplePrimitives:Array[Parse] = Array[Parse](
		NOW,
		MON(NOW),TUE(NOW),WED(NOW),THU(NOW),FRI(NOW),SAT(NOW),SUN(NOW),
		SEC,MIN,HOUR,DAY,WEEK,MONTH,YEAR
	)
	case class Feature(unigram:Int)
	
	val weights:Counter[(Feature,Int)] = new ClassicCounter[(Feature,Int)]


	override def parse(i:Int, sent:Sentence
			):(Array[Parse],(Int,Boolean,Double)=>Any)={
		assert(sent != null, "Sentence cannot be null")
		//--Features
		def features(sent:Sentence,out:Int):Counter[(Feature,Int)] = {
			val counts = new ClassicCounter[(Feature,Int)]
			sent.foreach((w:Int,t:Int)=>counts.incrementCount((Feature(w),out), 1.0))
			counts
		}
		def globalIndex(i:Int,p:Array[Parse]):Int = {
			simplePrimitives.zipWithIndex.foreach( (pair) => {
				val (parse,index) = pair
				if(parse == p(i)){ return index }
			})
			throw fail("Broke equality somewhere")
		}
		//--Score Sentence
		val parse:Array[Parse] = simplePrimitives.zipWithIndex.map( pair => {
				val (p,i) = pair
				val feats = features(sent,i)
				val score = Counters.dotProduct(feats,weights)
				(score,i)
			}).sortBy(_._1).reverse.map( (pair:(Double,Int)) => {
				simplePrimitives(pair._2)
			})
		//--Update
		val update = (index:Int,exact:Boolean,score:Double) => {
			if(exact){
				val feats = features(sent,globalIndex(index,parse))
				weights.addAll(feats)
			}
		}
		//--Return
		(parse, update)
	}
}


object Search {
}




