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
//-----
// Utilities
//-----
object ParseConversions {
	implicit def time2parse(t:Time):Parse = Parse(Range(t,t),null,null)
	implicit def range2parse(r:Range):Parse = Parse(r,null,null)
	implicit def duration2parse(d:Duration):Parse = Parse(null,d,null)
	implicit def fn2parse(fn:Range=>Range):Parse = Parse(null,null,fn)
}

//-----
// Sentence
//-----
case class Sentence(words:Array[Int],pos:Array[Int]) {
	def apply(i:Int) = words(i)
	def foreach(fn:(Int,Int)=>Any) = words.zip(pos).foreach(Function.tupled(fn))
	def length:Int = words.length
}

//-----
// Parse
//-----
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
		val groundedGold:Range = gold(grounding)
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

//-----
// Parse Traits
//-----
trait Parser {
	def report:Unit = {}
	def cycle(data:DataStore,iters:Int,feedback:Boolean=true):Array[Score]
	def run(data:Data,iters:Int):(Array[Score],Score) = {
		start_track("Training")
		val train = cycle(data.train,iters)
		end_track
		start_track("Testing")
		val test = cycle(if(O.devTest) data.dev else data.test, 1, false)(0)
		end_track
		start_track("Parser State")
		report
		end_track
		(train,test)
	}
}

trait StandardParser extends Parser {
	def parse(iter:Int, sent:Sentence, feedback:Boolean
		):(Array[Parse],(Int,Boolean,Double)=>Any)
	override def cycle(data:DataStore,iters:Int,feedback:Boolean):Array[Score] = {
		(1 to iters).map( (i:Int) => {
			start_track("Iteration " + i)
			val score = data.eachExample( (sent:Sentence) => {
				parse(i, sent, feedback)
			})
			log("Score: " + score)
			end_track
			score
		}).toArray
	}
}

//------------------------------------------------------------------------------
// TOY PARSERS
//------------------------------------------------------------------------------

class ItsAlwaysFriday extends StandardParser{
	override def parse(i:Int, sent:Sentence, feedback:Boolean
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


	override def parse(i:Int, sent:Sentence, feedback:Boolean
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

//------------------------------------------------------------------------------
// SEARCH PARSER
//------------------------------------------------------------------------------
object SearchParser {
//-----
// FEATURES
//-----
	trait Feature
	case class UnigramLexFeature(w:Int,lex:Int) extends Feature{
		override def toString:String 
			= ""+{if(lex<0) "NIL" else LEX(lex)}+"->"+U.w2str(w)
	}
	case class IndicatorRaiseFeature(raise:Int) extends Feature{
		override def toString:String = "raise"+raise
	}
//-----
// VALUES
//-----
	private val TYPE_RAISES = Array[(Symbol,(_ <: PartialParse)=>PartialParse)](
		//(ground fn(range,duration) to fn(NOW,duration))
//		('FunctionRangeDuration, 
//			(fn:(Range,Duration)=>Range) => { fn(Range(NOW,NOW),_:Duration) }:PartialParse),
		//(ground a duration to NOW)
		('Duration, (d:Duration) => { d(NOW) }:PartialParse)
	)
	private val LEX = Array[PartialParse](
		//(functions)
		shiftLeft,shiftRight,catLeft,catRight,shrinkBegin,shrinkEnd,intersect,cons,
		//(ranges)
		SEC,MIN,HOUR,DAY,WEEK,MONTH,QUARTER,YEAR,
		//(dow)
		MON,TUE,WED,THU,FRI,SAT,SUN,
		//(now)
		Range(NOW,NOW)
		)
	private val NIL = -1 //LEX index of the NIL term
	private val LEX_INDEX = LEX.zipWithIndex
	private val TYPE_RAISES_INDEX = TYPE_RAISES.zipWithIndex
}


class SearchParser extends StandardParser {
	import SearchParser._
	import scala.math.{min,max,log => ln,exp}
//-----
// BEHAVIOR
//-----
	private val weights:Counter[Feature] = new ClassicCounter[Feature]

	def raiseFeatures(state:State,raiseIndex:Int):Counter[Feature] = {
		val feats:Counter[Feature] = new ClassicCounter
		feats.incrementCount(IndicatorRaiseFeature(raiseIndex), 1.0)
		feats
	}
	def lexFeatures(s:Sentence,i:Int,lexIndex:Int):Counter[Feature] = {
		val feats:Counter[Feature] = new ClassicCounter
		feats.incrementCount(UnigramLexFeature(s.words(i),lexIndex), 1.0)
		feats
	}

	def feedback(feats:Counter[Feature],good:Boolean,score:Double) = {
		if(good){ 
			weights.addAll(feats)
		} else {
			weights.addAll(Counters.multiplyInPlace(feats,-1.0))
		}
	}
	def prob(feats:Counter[Feature]):Double = {
		1.0 / (1.0 + exp(-Counters.dotProduct(feats,weights)))
	}


//-----
// SEARCH STATE
//-----
	case class State(
			begin:Int,end:Int,
			parse:PartialParse,
			leftOf:PartialParse,rightOf:PartialParse,
			c:Double,
			updates:List[(Boolean,Double)=>Unit],
			sent:Sentence)
				extends SearchState {

		def realParse:Parse = {
			parse match {
				case t:Time => { Range(t,t) }
				case r:Range => { r }
				case d:Duration => { d }
				case fn:(Range=>Range) => { fn }
				case _:Any => {throw new SearchException("Invalid parse type")}
			}
		}

		override def cost:Double = -c

		override def children:List[State] = {
			var lst = List[State]()
			//--CASE: Unary
			if(begin >= 0 && end >= 0){
				TYPE_RAISES_INDEX.foreach( tuple => {
					type A = X forSome {type X <: PartialParse}
					val ((input,fn),raiseI):((Symbol,A=>PartialParse),Int) = tuple
					if(parse.typeTag == input){
						val newCost:Double = c+U.safeLn(prob(raiseFeatures(this,raiseI)))
						val update = (good:Boolean,d:Double) 
							=> feedback(raiseFeatures(this,raiseI),good,d)
						lst = this.copy(
							parse=fn(parse), c=newCost, updates=update :: updates) :: lst
					}
				})
			}

			//--CASE: Binary
//			if(begin >= 0 && end >= 0){
//				if(leftOf != null){
//					if(leftOf.accepts(parse.typeTag)){
//						val update = (good:Boolean,d:Double) 
//							=> feedback(applyFeatures(leftOf,parse.typeTag,),good,d)
//						val newCost:Double =c+U.safeLn(prob(lexFeatures(sent,begin-1,lexI)))
//						lst = this.copy(leftOf=p, c=newCost, updates=update::updates) :: lst
//						
//					}
//				}
//			}

			//--CASE: Tokenize
			if(begin >= 0 && end >= 0){
				//--Case: Add Token
				LEX_INDEX.foreach( (pair:(PartialParse,Int)) => {
					val (p,lexI) = pair
					//(add to left)
					if(begin > 0 && leftOf == null){
						val update = (good:Boolean,d:Double) 
							=> feedback(lexFeatures(sent,begin-1,lexI),good,d)
						val newCost:Double =c+U.safeLn(prob(lexFeatures(sent,begin-1,lexI)))
						lst = this.copy(leftOf=p, c=newCost, updates=update::updates) :: lst
					}
					//(add to right)
					if(end < (sent.length-1) && rightOf == null){
						val update = (good:Boolean,d:Double) 
							=> feedback(lexFeatures(sent,end+1,lexI),good,d)
						val newCost:Double = c+U.safeLn(prob(lexFeatures(sent,end+1,lexI)))
						lst = this.copy(rightOf=p, c=newCost, updates=update::updates) ::lst
					}
				})
			} else {
				//--Case: First Token
				for(index <- 0 to sent.length-1){
					LEX_INDEX.foreach( (pair:(PartialParse,Int)) => {
						val (p,lexI) = pair
						val update = (good:Boolean,d:Double) 
							=> feedback(lexFeatures(sent,index,lexI),good,d)
						val newCost:Double = c+U.safeLn(prob(lexFeatures(sent,index,lexI)))
						lst = State(index,index+1,p,leftOf,rightOf,
							newCost,update :: updates,sent) :: lst
					})
				}
			}
			//--Case: nil Expand
			if(leftOf == null && begin > 0){
				val newCost:Double = c + U.safeLn(prob(lexFeatures(sent,begin-1,NIL)))
				val update = (good:Boolean,d:Double) 
					=> feedback(lexFeatures(sent,begin-1,NIL),good,d)
				lst = this.copy(begin=begin-1,c=newCost, updates=update::updates) :: lst
			}
			if(rightOf == null && end < sent.length-1){
				val newCost:Double = c + U.safeLn(prob(lexFeatures(sent,end+1,NIL)))
				val update = (good:Boolean,d:Double) 
					=> feedback(lexFeatures(sent,end+1,NIL),good,d)
				lst = this.copy(end=end+1, c=newCost, updates=update::updates) :: lst
			}
			//--Return
			lst
		}
		override def isEndState:Boolean 
			= begin == 0 && end == sent.length && 
				parse != null && {parse match {
					case t:Time => { true }
					case r:Range => { true }
					case d:Duration => { true }
					case fn:(Range=>Range) => { fn.typeTag == 'FunctionRange }
					case _:Any => {false}
				}}
	
		override def assertDequeueable:Boolean = {
//			println("Dequeued " + U.sent2str(sent.words) + " ==> " + this)
			true
		}
		override def assertEnqueueable:Boolean = {
			true
		}
		
		override def toString:String 
			= ""+parse+"["+begin+","+end+"):"+G.df.format(cost)
	}
	object State {
		def start(sent:Sentence):State =
			State(-1,-1,null,null,null,0.0,List[(Boolean,Double)=>Unit](),sent)
	}


//-----
// PARSE
//-----
	override def report:Unit = {
		val weightQueue = Counters.toPriorityQueue(weights)
		start_track("top weights")
		for(i <- 1 to min(10,weightQueue.size)){
			val priority = weightQueue.getPriority
			logG("" + weightQueue.removeFirst + " -> " + priority)
		}
		end_track
	}

	// -- Parse --
	override def parse(i:Int, sent:Sentence, feedback:Boolean
			):(Array[Parse],(Int,Boolean,Double)=>Any)={
		import Search._
		//--Parse
		var parseLst:List[State] = List[State]()
		val search:Search[State] = Search(memcap(UNIFORM_COST,200000,0.5))
		search.search(
			State.start(sent),
			(parse:State,iter:Int) => {
				parseLst = parse :: parseLst
				true
			},
			O.maxSearchTime)
		val parses:Array[State] = parseLst.reverse.toArray
		//--Update (perceptron)
		val update = (index:Int,exact:Boolean,score:Double) => {
			if(exact && index != 0){ //something is right, and 
				val gold = parses(index)
				val guess = parses(0)
				gold.updates.foreach( _(true,score) )
				guess.updates.foreach( _(false,score) )
			}
		}
		//--Debug
		log("Parsed \"" + U.sent2str(sent.words) + "\" as " + 
			U.join(parses.slice(0,5).map( _.realParse ), " or "))
		(parses.map( _.realParse ), if(feedback){ update } else { (i,e,s)=>{} } )
	}
}







