package time

import scala.collection.JavaConversions._
import scala.collection.mutable.PriorityQueue

import Lex._
import Conversions._
import ParseConversions._

import org.goobs.exec.Log._
import org.goobs.exec.Execution

import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.stats.Counters;


//------------------------------------------------------------------------------
// GRAMMAR
//------------------------------------------------------------------------------
object Head extends Enumeration {
	type V = Value
	val ROOT, Word, Time, Range, Duration, F_RR, F_RD, F_R, F_D = Value
}
trait Rule {
	def apply(arg:Any):Any
	def apply(arg1:Any, arg2:Any):Any
	def arity:Int
	def head:Head.Value
	def accepts(a:Head.Value):Boolean
	def accepts(a:Head.Value,b:Head.Value):Boolean

	private var leftChild:Head.Value = null
	private var rightChild:Head.Value = null
	private def cacheRule:Unit = {
		if(leftChild != null){ return; }
		if(arity == 1){
			Head.values.foreach{ (child:Head.Value) =>
				if(accepts(child)){
					assert(leftChild == null, "Multiple accepted inputs for rule")
					this.leftChild = child
				}
			}
		} else if(arity == 2){
			Head.values.foreach{ (left:Head.Value) =>
				Head.values.foreach{ (right:Head.Value) =>
					if(accepts(left,right)){
						assert(leftChild==null && rightChild==null,
							"Multiple accepted inputs for rule")
						this.leftChild = left
						this.rightChild = right
					}
				}
			}
		} else {
			throw new IllegalStateException("Left on invalid arity rule")
		}
		assert(leftChild != null, "No accepted inputs for rule")
		assert(arity == 1 || rightChild != null, "No accepted inputs for rule")
	}
	def left:Head.Value = { cacheRule; return leftChild; }
	def right:Head.Value = {
		 assert(arity == 2, "Bad arity"); cacheRule; return rightChild;
	}
	def child:Head.Value  = {
		assert(arity == 1, "Bad arity"); cacheRule; return leftChild;
	}

	def signature:String = {
		val children:String = if(arity == 1){
				val a:Array[Head.Value] = Head.values.filter( v => accepts(v) ).toArray
				a(0).toString
			} else {
				var str = "<unknown>"
				Head.values.foreach( v1 => {
					Head.values.foreach( v2 => {
						if(accepts(v1,v2)){
							str = "" + v1 + "," + v2
						}
					})
				})
				str
			}
		head.toString + "->" + children
	}
}

case class UnaryRule(
		out:Head.Value,
		in:Head.Value,
		fn:Any=>Any ) extends Rule {
	override def apply(arg:Any):Any = fn(arg)
	override def apply(arg1:Any,arg2:Any) 
		= throw fail("binary apply to unary rule")
	def arity:Int = 1
	def head:Head.Value = out
	def accepts(a:Head.Value):Boolean = a == in
	def accepts(a:Head.Value,b:Head.Value):Boolean = false
	override def toString:String =
		""+out+{if(in==Head.Word) "["+this(0)+"]" else ""}+"->"+in
}
case class BinaryRule(
		out:Head.Value,
		in1:Head.Value,
		in2:Head.Value,
		fn:(Any,Any)=>Any) extends Rule {
	override def apply(arg:Any)
		= throw fail("unary apply to binary rule")
	override def apply(arg1:Any,arg2:Any):Any = fn(arg1,arg2)
	def arity:Int = 2
	def head:Head.Value = out
	def accepts(a:Head.Value):Boolean = false
	def accepts(a:Head.Value,b:Head.Value):Boolean = (a == in1) && (b == in2)
	override def toString:String =
		""+out+"->"+in1+","+in2
}

object Grammar {
	
	val RULES:Array[Rule] = {
		def hack[A,Z](fn:A=>Z):Any=>Any = fn.asInstanceOf[Any=>Any]
		def hack2[A,B,Z](fn:(A,B)=>Z):(Any,Any)=>Any 
			= fn.asInstanceOf[(Any,Any)=>Any]
		var rtn = List[Rule]()

		//--Lex Terms
		//(times)
		val times = List[Time](NOW)
		rtn = rtn ::: times.map( (t:Time) => 
			UnaryRule(Head.Time, Head.Word, hack((w:Int) => t)))
		//(ranges)
		val ranges = List[Range]()
		rtn = rtn ::: ranges.map( (r:Range) => 
			UnaryRule(Head.Range, Head.Word, hack((w:Int) => r)))
		//(durations)
		val durations = 
			List[Duration](SEC,MIN,HOUR,DAY,WEEK,MONTH,QUARTER,YEAR) :::
			(1 to 7).map( i => DOW(i) ).toList :::
			(1 to 12).map( i => MOY(i) ).toList :::
			(1 to 4).map( i => QOY(i) ).toList
		rtn = rtn ::: durations.map( (d:Duration) => 
			UnaryRule(Head.Duration, Head.Word, hack((w:Int) => d)))

		//--Type Raises
		//(range introduction)
		rtn = UnaryRule(Head.Range, Head.Time, hack( 
				(t:Time) => Range(t,t)
			)) :: rtn
		//(now augmentation)
		rtn = UnaryRule(Head.F_D, Head.F_RD, hack( 
				(f:(Range,Duration)=>Range) => f(Range(NOW,NOW),_:Duration) 
			)) :: rtn
		//(implicit intersect)
		rtn = UnaryRule(Head.F_R, Head.Range, hack(
				(r:Range) => intersect(r,_:Range)
			)) :: rtn
		//(sequence grounding)
		rtn = UnaryRule(Head.Range, Head.Duration, hack( 
				(d:Duration) => d(NOW)
			)) :: rtn

		//--F[ Range, Duration ]
		val rangeDurationFn = List[(Range,Duration)=>Range](
			shiftLeft,shiftRight,catLeft,catRight,shrinkBegin,shrinkEnd)
		//(intro)
		rtn = rtn ::: rangeDurationFn.map( (fn:(Range,Duration)=>Range) => //intro
			UnaryRule(Head.F_RD, Head.Word, hack((w:Int) => fn)))
		//(right apply)
		rtn = rtn ::: rangeDurationFn.map( (fn:(Range,Duration)=>Range) => //intro
			BinaryRule(Head.F_R, Head.F_RD, Head.Duration, hack2(
				(fn:(Range,Duration)=>Range,d:Duration) => fn(_:Range,d)
				)))
		//(left apply)
		rtn = rtn ::: rangeDurationFn.map( (fn:(Range,Duration)=>Range) => //intro
			BinaryRule(Head.F_R, Head.Duration, Head.F_RD, hack2(
				(d:Duration,fn:(Range,Duration)=>Range) => fn(_:Range,d)
				)))

		//--F[ Range, Range ]
		val rangeRangeFn = List[(Range,Range)=>Range](
			intersect,cons)
		//(intro)
		rtn = rtn ::: rangeRangeFn.map( (fn:(Range,Range)=>Range) =>  //intro
			UnaryRule(Head.F_RR, Head.Word, hack((w:Int) => fn)))
		//(right apply)
		rtn = rtn ::: rangeRangeFn.map( (fn:(Range,Range)=>Range) => //intro
			BinaryRule(Head.F_R, Head.F_RR, Head.Range, hack2(
				(fn:(Range,Range)=>Range,r:Range) => fn(_:Range,r)
				)))
		//(left apply)
		rtn = rtn ::: rangeRangeFn.map( (fn:(Range,Range)=>Range) => //intro
			BinaryRule(Head.F_R, Head.Range, Head.F_RR, hack2(
				(r:Range,fn:(Range,Range)=>Range) => fn(r,_:Range)
				)))

		//--F[ Range ]
		rtn = rtn ::: List[BinaryRule](
			//(right apply)
			BinaryRule(Head.Range, Head.F_R, Head.Range, hack2(
				(fn:Range=>Range,r:Range) => fn(r)
				)),
			//(left apply)
			BinaryRule(Head.Range, Head.Range, Head.F_R, hack2(
				(r:Range,fn:Range=>Range) => fn(r)
				))
			)

		//-ROOT
		rtn = rtn ::: List[UnaryRule](
			UnaryRule(Head.ROOT, Head.Time, hack((t:Time) => t)),
			UnaryRule(Head.ROOT, Head.Range, hack((r:Range) => r)),
			UnaryRule(Head.ROOT, Head.Duration, hack((d:Duration) => d)),
			UnaryRule(Head.ROOT, Head.F_R, hack((fn:Range=>Range) => fn))
			)
		//--Return
		rtn.toArray
	}
	val UNARIES:Array[(Rule,Int)]  = RULES.zipWithIndex.filter{ _._1.arity == 1 }
	val BINARIES:Array[(Rule,Int)] = RULES.zipWithIndex.filter{ _._1.arity == 1 }
	val RULES_INDEX = RULES.zipWithIndex

	case class Closure(head:Head.Value,child:Head.Value,rules:Array[Int])

	private def computeClosures(raw:Array[Rule]):Array[Closure] = {
		//--Construct Graph
		case class Node(head:Head.Value,var neighbors:List[(Node,Int)]){
			def this(head:Head.Value) = this(head,List[(Node,Int)]())
			def addNeighbor(n:Node,ruleI:Int) = { neighbors = (n,ruleI)::neighbors }
			def search(seen:Array[Boolean],backtrace:List[Int],
					tick:(Head.Value,List[Int])=>Any):Unit = {
				//(overhead)
				if(seen(head.id)){ throw new IllegalStateException("Cyclic unaries") }
				seen(head.id) = true
				//(report path)
				if(backtrace.length > 0){ tick(head,backtrace) }
				//(continue searching
				neighbors.foreach{ case (node,rid) =>
					node.search(seen,rid :: backtrace,tick)
				}
			}
		}
		val graph = Head.values.map{ new Node(_) }.toArray
		UNARIES.foreach{ case (r,rid) => 
			graph(r.head.id).addNeighbor(graph(r.child.id),rid) 
		}
		//--Search Graph
		var closures = List[Closure]()
		graph.foreach{ (start:Node) => 
			start.search(new Array[Boolean](graph.length), List[Int](),
				(child:Head.Value,rules:List[Int]) => {
					closures = Closure(start.head,child,rules.reverse.toArray) :: closures
				})
		}
		closures.toArray
	}
	val CLOSURES:Array[Closure] = computeClosures(RULES)
	val CLOSURES_INDEX:Array[(Closure,Int)] = CLOSURES.zipWithIndex

	val NIL = -1 //LEX index of the NIL term
}

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
// Input / Output
//-----
case class Sentence(words:Array[Int],pos:Array[Int]) {
	def apply(i:Int) = words(i)
	def foreach(fn:(Int,Int)=>Any) = words.zip(pos).foreach(Function.tupled(fn))
	def length:Int = words.length
	override def toString:String = U.sent2str(words)
}
case class Feedback(correct:Array[(Int,Double)],incorrect:Array[(Int,Double)]) {
	def hasCorrect:Boolean = correct.length > 0
	def bestIndex:Int = correct(0)._1
	def wasWrong:Boolean = (!hasCorrect || bestIndex != 0)
	def correctCount:Int = correct.length
	def correctCountDbl:Double = correctCount.asInstanceOf[Double]
}

trait Tree[A]{
	def head:A
	def children:Array[_<:Tree[A]]
	def isLeaf:Boolean = (children.length == 0)
	private def prettyPrintAppend(printer:A=>String,b:StringBuilder):Unit = {
		b.append("(").append(printer(head))
		children.foreach( (tree:Tree[A]) => {
			b.append(" ")
			tree.prettyPrintAppend(printer,b)
		})
		b.append(")")
	}
	def prettyPrint(printer:A=>String = _.toString):String = {
		val b = new StringBuilder
		prettyPrintAppend(printer,b)
		b.toString
	}
}
trait ParseTree extends Tree[Head.Value] {
	override def children:Array[ParseTree]
	def evaluate(sent:Sentence):(Head.Value,Any)
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
		):(Array[Parse],Feedback=>Any)
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
			):(Array[Parse],Feedback=>Any)={
		val parse:Array[Parse] = Array[Parse](
			FRI(NOW),                              // I think it's friday
			(r:Range) => Range(r.begin,NOW),       // or 'the past'
			WEEK,                                  // or a week
			Range(Time(2011,4,26),Time(2011,4,27)) // or April 26
			)
		(parse, (feedback:Feedback) => {})
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
			):(Array[Parse],Feedback=>Any)={
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
		val update = (feedback:Feedback) => {
			if(feedback.hasCorrect){
				val feats = features(sent,globalIndex(feedback.bestIndex,parse))
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
	import Grammar._
//-----
// INTERACTIVE
//-----
	def interactive:Unit = {
		var cont:Boolean = true
		while(cont){
			val line = Console.readLine("search> ")
			println(line)
		}
	}
//-----
// FEATURES
//-----
	trait Feature
	case class UnigramLexFeature(w:Int,lex:Int) extends Feature {
		override def toString:String 
			= ""+{if(lex<0) "NIL_INTRO" else RULES(lex).toString}+"["+U.w2str(w)+"]"+
				":("+lex+","+w+")"
	}
	case class IndicatorRuleFeature(rule:Int) extends Feature {
		override def toString:String = RULES(rule).signature
	}
}


class SearchParser extends StandardParser {
	import SearchParser._
	import scala.math.{min,max,log => ln,exp}
	import Grammar._
//-----
// BEHAVIOR
//-----
	protected val weights:Counter[Feature] = new ClassicCounter[Feature]

	def ruleFeatures(state:State,ruleIndex:Int):Counter[Feature] = {
		val feats:Counter[Feature] = new ClassicCounter
		feats.incrementCount(IndicatorRuleFeature(ruleIndex), 1.0)
		feats
	}
	def lexFeatures(s:Sentence,i:Int,lexIndex:Int):Counter[Feature] = {
		val feats:Counter[Feature] = new ClassicCounter
		feats.incrementCount(UnigramLexFeature(s.words(i),lexIndex), 1.0)
		feats
	}

	def feedback(feats:Counter[Feature],good:Boolean,score:Double):Unit = {
		if(good){ 
			val posCounts:Counter[Feature] = new ClassicCounter[Feature]
			feats.keySet.foreach( (f:Feature) => {
				posCounts.incrementCount(f,score*feats.getCount(f))
			})
			weights.addAll(posCounts)
		} else {
			val negCounts:Counter[Feature] = new ClassicCounter[Feature]
			feats.keySet.foreach( (f:Feature) => {
				negCounts.incrementCount(f,-score*feats.getCount(f))
			})
			weights.addAll(negCounts)
		}
	}
	def score(feats:Counter[Feature]):Double = {
//		-U.safeLn(1.0 / (1.0 + exp(-Counters.dotProduct(feats,weights))))
		-Counters.dotProduct(feats,weights)
	}


//-----
// SEARCH STATE
//-----
	case class State(
			begin:Int,end:Int,
			parse:(Head.Value,Any),
			leftOf:(Head.Value,Any),rightOf:(Head.Value,Any),
			c:Double,
			updates:List[(Boolean,Double)=>Unit],
			sent:Sentence
				) extends SearchState with Ordered[State]{
		
		def realParse:Parse = {
			val (parseType, parseValue) = parse
			assert(parseType == Head.ROOT, "No parse for non-root node")
			parseValue match{
				case (t:Time) => Parse(Range(t,t),null,null)
				case (r:Range) => Parse(r,null,null)
				case (d:Duration) => Parse(null,d,null)
				case (fn:(Range=>Range)) => Parse(null,null,fn)
				case _ => throw fail("Unknown parse output: " + parseValue)
			}
		}

		override def isEndState = {
			begin <= 0 && end >= sent.length && parse._1 == Head.ROOT
//			if(begin <= 0 && end >= sent.length){
//				parse._1 match{
//					case Head.Time => true
//					case Head.Range => true
//					case Head.Duration => true
//					case Head.F_R => true
//					case _ => false
//				}
//			} else {
//				false
//			}
		}

		override def children:List[State] = {
			var children = List[State]()
			val started = parse != null
			//--Start
			if(!started){
				for( index <- 0 until RULES.length ){ //for each rule...
					val rule:Rule = RULES(index)
					if(rule.arity == 1 && rule.accepts(Head.Word)){ //..that's lexical
						for(wordI <- 0 until sent.length){
							val feats = lexFeatures(sent,wordI,index)
							val up = feedback(feats,_:Boolean,_:Double)
							children = State(
								wordI, wordI+1,
								(rule.head, rule(sent(wordI))),
								null,null,
								score(feats),
								List[(Boolean,Double)=>Unit](up),
								sent) :: children
						}
					}
				}
			}
			//--Tag
			//(tag left)
			if(started && begin > 0 && leftOf == null){
				for( index <- 0 until RULES.length ){
					val rule:Rule = RULES(index)
					if(rule.arity == 1 && rule.accepts(Head.Word)){
						//(add rule)
						val feats = lexFeatures(sent,begin-1,index)
						val up = feedback(feats,_:Boolean,_:Double)
						children = this.copy(
							leftOf=(rule.head, rule(sent(begin-1))), 
							c=c+score(feats),
							updates=up::updates
							) :: children
					}
				}
			}
			//(tag right)
			if(started && end < sent.length-1 && rightOf == null){
				for( index <- 0 until RULES.length ){
					val rule:Rule = RULES(index)
					if(rule.arity == 1 && rule.accepts(Head.Word)){
						//(add rule)
						val feats = lexFeatures(sent,end+1,index)
						val up = feedback(feats,_:Boolean,_:Double)
						children = this.copy(
							rightOf=(rule.head, rule(sent(end+1))), 
							c=c+score(feats),
							updates=up::updates
							) :: children
					}
				}
			}
			//--Rules
			if(started){
				for( index <- 0 until RULES.length ){
					val rule:Rule = RULES(index)
					if( rule.arity == 1 && rule.accepts(parse._1) ){
						//(type raise)
						val feats = ruleFeatures(this,index)
						val up = feedback(feats,_:Boolean,_:Double)
						children = this.copy(
								parse=(rule.head,rule(parse._2)),
								c=c+score(feats),
								updates=up::updates
								) :: children
					}else if(rule.arity == 2){
						assert(begin>0 || leftOf==null, "moved too far left")
						assert(end< sent.length || rightOf==null, "moved too far right")
						val (parseType, parseValue) = parse
						//(left apply)
						if(leftOf != null){
							val (nodeType, nodeValue) = leftOf
							if(rule.accepts(nodeType, parseType)){
								val feats = ruleFeatures(this,index)
								val up = feedback(feats,_:Boolean,_:Double)
								children = this.copy(
										parse=(rule.head,rule(nodeValue, parseValue)),
										c=c+score(feats),
										updates=up::updates,
										leftOf=null,
										begin=begin-1
										) :: children
							}
						}
						//(right apply)
						if(rightOf != null){
							val (nodeType, nodeValue) = rightOf
							if(rule.accepts(parseType, nodeType)){
								val feats = ruleFeatures(this,index)
								val up = feedback(feats,_:Boolean,_:Double)
								children = this.copy(
										parse=(rule.head,rule(parseValue, nodeValue)),
										c=c+score(feats),
										updates=up::updates,
										rightOf=null,
										end=end+1
										) :: children
							}
						}
					} else if(rule.arity > 2 || rule.arity < 1){
						//(strange rule...)
						throw fail("Arity > 2 rule")
					}
				}
			}
			//--Nil Introduction
			if(started && leftOf == null && begin > 0){
				val feats = lexFeatures(sent,begin-1,NIL)
				val up = feedback(feats,_:Boolean,_:Double)
				children = this.copy(
					begin=begin-1,
					c=c+score(feats),
					updates=up::updates
					) :: children
			}
			if(started && rightOf == null && end < sent.length-1){
				val feats = lexFeatures(sent,end+1,NIL)
				val up = feedback(feats,_:Boolean,_:Double)
				children = this.copy(
					end=end+1,
					c=c+score(feats),
					updates=up::updates
					) :: children
			}
			//--Return
			children
		}
		override def cost:Double = c
		override def assertDequeueable:Boolean = {
			true
		}
		override def assertEnqueueable:Boolean = {
			true
		}
		override def compare(s:State) = {
			if(this.cost > s.cost){        1 }
			else if(this.cost < s.cost){  -1 }
			else{                          0 }
		}
		override def toString:String = {
			if(parse == null){
				"START"
			} else {
				""+G.df.format(cost)+" ["+begin+"-"+end+")("+parse._1+")"+parse._2
			}
		}
	}
	
	object State {
		def start(sent:Sentence):State =
			State(-1,-1,null,null,(null,null),0.0,List[(Boolean,Double)=>Unit](),sent)
	}


//-----
// PARSE
//-----
	override def report:Unit = {
		val weightQueue = Counters.toPriorityQueue(weights)
		val writer = new java.io.FileWriter(Execution.touch("weights"))
		//--Print+File
		start_track("top weights")
		for(i <- 1 to min(10,weightQueue.size)){
			val priority = weightQueue.getPriority
			val msg = "["+priority+"] " + weightQueue.removeFirst
			logG(msg)
			writer.write(msg); writer.write("\n")
		}
		//--File
		logG("" + weightQueue.size + " more...")
		for(i <- 1 to weightQueue.size){
			val priority = weightQueue.getPriority
			val msg = "["+priority+"] " + weightQueue.removeFirst
			writer.write(msg); writer.write("\n")
		}
		end_track
		writer.close
	}

	// -- Parse --
	override def parse(i:Int, sent:Sentence, feedback:Boolean
			):(Array[Parse],Feedback=>Any)={
		import Search._
		//--Parse
		var parseLst:List[State] = List[State]()
		val search:Search[State] = Search(memcap(UNIFORM_COST,O.beam*2,0.5))
		search.search(
			State.start(sent),
			(parse:State,iter:Int) => {
				parseLst = parse :: parseLst
				true
			},
			O.maxSearchTime)
		val parses:Array[State] = parseLst.sortWith(_<_).toArray
		//--Update (perceptron)
		val update = (fb:Feedback) => {
			if(fb.wasWrong && fb.correctCount > 0){
				val guess = parses(0)
				//(increment gold parses)
				parses(fb.correct(0)._1).updates.foreach(_(true,1.0))
//				fb.correct.foreach( (pair:(Int,Double)) => {
//					val (index,score) = pair
//					val gold = parses(index)
//					gold.updates.foreach(_(true,1.0/fb.correctCountDbl))
//				})
				//(decrement guess parse)
				guess.updates.foreach(_(false,1.0))
			}
		}
		//--Debug
		log("Parsed \"" + U.sent2str(sent.words) + "\" ("+parses.length+") as " + 
			U.join(
				parses.slice(0,1).map(
					p => ""+p.realParse+"["+G.df.format(p.cost)+"]"), " or "))
		(parses.map( _.realParse ), if(feedback){ update } else { fb=>{} } )
	}
}


//------------------------------------------------------------------------------
// CKY PARSER
//------------------------------------------------------------------------------
class CKYParser extends StandardParser{
	import Grammar._

	val ckyK = O.beam

	//-----
	// Elem / Tree / Derivation
	//-----
	class ChartElem(
			var logScore:Double, 
			var ruleI:Int, 
			var left:ChartElem,
			var right:ChartElem) extends ParseTree {
		// -- CKY Properties --
		def this() = this(Double.NegativeInfinity,-1,null,null)
		def this(logScore:Double,ruleI:Int) = this(logScore,ruleI,null,null)
		def apply(logScore:Double,ruleI:Int,left:ChartElem,right:ChartElem
				):ChartElem = {
			val rule = RULES(ruleI)
			assert(rule.arity == 1 || rule.arity == 2, "Bad rule arity")
			this.logScore = logScore
			this.ruleI = ruleI
			this.left = left
			this.right = right
			this
		}
		def apply(logScore:Double,ruleI:Int,left:ChartElem):ChartElem = {
			assert(RULES(ruleI).arity == 1, "Invalid apply for arity 1 rule")
			apply(logScore,ruleI,left,null)
		}
		def apply(other:ChartElem):ChartElem = {
			assert(!other.isNil, "Setting to nil chart element")
			apply(other.logScore,other.ruleI,other.left,other.right)
		}
		def nilify:Unit = { logScore = Double.NaN; ruleI = -1 }
		def isNil:Boolean = (ruleI < 0)
		// -- ParseTree Properties --
		override def head:Head.Value 
			= { assert(ruleI>=0,"taking head of null rule"); RULES(ruleI).head }
		override def children:Array[ParseTree] = {
			assert(ruleI>=0,"taking children of null rule")
			val rule = RULES(ruleI)
			if(left == null && right == null) { //leaf
				return Array[ParseTree]()
			} else if(rule.arity == 1){
				Array[ParseTree](left)
			} else if(rule.arity == 2) {	
				Array[ParseTree](left,right)
			} else {
				throw new IllegalStateException("Bad Rule Arity")
			}
		}
		private def evaluateHelper(sent:Sentence,i:Int):(Int,(Head.Value,Any)) = {
			assert(ruleI>=0,"evaluating null rule")
			val rule = RULES(ruleI)
			if(isLeaf){
				//(base case)
				(i+1,(rule.head,rule(sent.words(i))))
			}else{
				//(recursive case)
				if(rule.arity == 1){
					val (childI,(childType,childValue)) = evaluateHelper(sent,i)
					(childI,(rule.head,rule(childValue)))
				} else if(rule.arity == 2) {	
					val (leftI,(leftType,leftValue)) = evaluateHelper(sent,i)
					val (rightI,(rightType,rightValue)) = evaluateHelper(sent,leftI)
					(rightI,(rule.head,rule(leftValue,rightValue)))
				} else {
					throw new IllegalStateException("Bad Rule Arity")
				}
			}
		}
		override def evaluate(sent:Sentence):(Head.Value,Any) = {
			val (length,output) = evaluateHelper(sent,0)
			assert(length == sent.length)
			output
		}
		// -- Object Properties --
		override def clone:ChartElem = {
			new ChartElem(logScore,ruleI,left,right)
		}
		override def equals(a:Any) = {
			a match {
				case (elem:ChartElem) => {
					elem.ruleI == ruleI && elem.left == left && elem.right == right
				}
				case (_:Any) => false
			}
		}
	}

	//-----
	// K-Best List
	//-----
	class BestList(values:Array[ChartElem]) {

		// -- Structure --
		var length = 0
		def apply(i:Int) = values(i)
		def capacity:Int = values.length
		def reset:Unit = {
			values(0).nilify
			length = 0
		}
		def foreach(fn:ChartElem=>Any):Unit = {
			for(i <- 0 until length){ fn(values(i)) }
		}
		def map[A : Manifest](fn:ChartElem=>A):Array[A] = {
			val rtn = new Array[A](length)
			for(i <- 0 until length){
				rtn(i) = fn(values(i))
			}
			rtn
		}
		def zipWithIndex = values.slice(0,length).zipWithIndex
		def toArray:Array[ChartElem] = {
			values.slice(0,length)
		}
		override def clone:BestList = {
			var rtn = new BestList(values.clone)
			rtn.length = this.length
			rtn
		}
		def deepclone:BestList = {
			var rtn = new BestList(values.map{ _.clone })
			rtn.length = this.length
			rtn
		}

		// -- As Per (Huang and Chiang 2005) --
		//<Paranoid Checks>
		private def check(nonempty:Boolean=true):(Boolean,String) = {
			//(non-empty)
			if(nonempty && length == 0){ return (false,"empty") }
			//(non-null)
			for(i <- 0 until this.length){
				if(values(i).isNil){ return (false,"nil element at " + i) }
			}
			//(non-infinite score)
			for(i <- 0 until this.length){
				if(values(i).logScore == Double.NegativeInfinity ||
						values(i).logScore == Double.PositiveInfinity ||
						values(i).logScore.isNaN ){ 
					return (false,"bad score for element " + i)
				}
			}
			//(sorted)
			var last:Double = Double.PositiveInfinity
			for(i <- 0 until this.length){
				if(last < values(i).logScore){ return (false,"not sorted") }
				last = values(i).logScore
			}
			//(unique)
			for(i <- 0 until this.length) {
				for(j <- (i+1) until this.length) {
					if(values(i).equals(values(j))){ return (false,"not unique") }
				}
			}
			//(ok)
			return (true,"")
		}

		//<Algorithm 0>
		private def mult0(ruleI:Int, left:BestList, right:BestList,
				score:(ChartElem,ChartElem)=>Double
				):Array[(Double,ChartElem,ChartElem)]= {
			//--Create Combined List
			val combined:Array[(Double,ChartElem,ChartElem)] = if(right != null){
				//(case: binary rule)
				assert(left.length > 0 && right.length > 0, "bad length")
				val out 
					= new Array[(Double,ChartElem,ChartElem)](left.length*right.length)
				for( lI <- 0 until left.length ){
					for(rI <- 0 until right.length ){
						out(right.length*lI + rI) 
							= (left(lI).logScore+right(rI).logScore+score(left(lI),right(rI)),
							   left(lI),
								 right(rI))
					}
				}
				out
			} else {
				//(case: unary rule)
				assert(left.length > 0, "bad length")
				left.map{ elem => 
					(elem.logScore+score(elem,null), elem, null)
				}
			}
			//--Sort List
			combined.sortBy( - _._1 )
			assert(combined.length > 0, "empty combined vector")
			combined
		}
		private def merge0(ruleI:Int, 
				input:Array[(Double,ChartElem,ChartElem)]):Unit = {
			assert(ruleI >= 0, "Merging bad rule")
			assert(capacity > 0 && (this.length > 0 || input.length > 0),
				"bad precondition to merge")
			var defendP = 0
			var candP = 0
			var index:Int = 0
			val defender = this.deepclone
			while(index < capacity && 
					(defendP < this.length ||
					candP < input.length) ){
				//(get candidate scores)
				val defend:Double
						= if(defendP < this.length) defender(defendP).logScore 
						else Double.NegativeInfinity
				val cand:Double
						= if(candP < input.length) input(candP)._1
						else Double.NegativeInfinity
				assert(cand > Double.NegativeInfinity || 
					defend > Double.NegativeInfinity,
					"No acceptable scores")
				//(set largest element)
				if(cand > defend){
					//(case: take new)
					val (score,left,right) = input(candP)
					candP += 1
					if(right == null) {
						assert(left != null, "setting to null rule")
						values(index)(score,ruleI,left)
					} else {
						assert(left != null, "setting to null rules")
						values(index)(score,ruleI,left,right)
					}
				} else {
					//(case: keep old)
					if(O.paranoid)
						{ val (ok,str) = defender.check(false); assert(ok,"merge: " +str) }
					assert(defendP < defender.length, "defendP is larger than length")
					assert(defend > Double.NegativeInfinity, "score should be valid")
					assert(!defender(defendP).isNil, "setting to nil element!")
					values(index)(defender(defendP))
					defendP += 1
				}
				//(increment index)
				index += 1
			}
			//(set length)
			length = index
			assert(length != 0, "Merge returned length 0")
		}
		private def algorithm0(ruleI:Int, left:BestList, right:BestList,
				score:(ChartElem,ChartElem)=>Double):Unit = {
			assert(left.length > 0, "precondition for algorithm0")
			merge0(ruleI,mult0(ruleI, left, right, score))
		}

		//<Top Level>
		def combine(ruleI:Int, left:BestList, right:BestList,
				score:(ChartElem,ChartElem)=>Double):Unit = {
			if(left.length > 0 && (right == null || right.length > 0)){
				//(pre)
				var save:BestList = if(O.paranoid){ this.clone } else { null }
				if(O.paranoid){ val (ok,str) = check(false); assert(ok,"pre: " +str) }
				//(execute)
				this.algorithm0(ruleI, left, right, score) //<--Execute
				//(checks)
				if(O.paranoid){ 
					//(sanity)
					val (ok,str) = check(); assert(ok,"post: " + str)
					//(correctness)
					save.algorithm0(ruleI, left, right, score)
					assert(save.length == this.length, "length is wrong")
					for(i <- 0 until length){
						assert(save(i).equals(this(i)), "element " + i + " is wrong")
					}
				}
			}
		}
		def combine(ruleI:Int, left:BestList,
				score:(ChartElem,ChartElem)=>Double):Unit = {
			assert(RULES(ruleI).arity == 1, "must be arity 1 rule")
			combine(ruleI, left, null, score)
		}

		// -- Standard Methods --
		def add(score:Double,ruleI:Int,left:ChartElem,right:ChartElem) = {
			assert(RULES(ruleI).arity == 2, "must be arity 2 rule")
			values(length)(score,ruleI,left,right)
			length += 1
		}
		def add(score:Double,ruleI:Int,left:ChartElem) = {
			assert(RULES(ruleI).arity == 1, "must be arity 1 rule")
			values(length)(score,ruleI,left)
			length += 1
		}
		def suggest(score:Double,ruleI:Int,left:ChartElem,right:ChartElem) = {
			if(length < capacity){ add(score,ruleI,left,right) }
		}
		def suggest(score:Double,ruleI:Int,left:ChartElem) = {
			if(length < capacity){ add(score,ruleI,left) }
		}
		def suggest(score:Double,ruleI:Int) = {
			if(length < capacity){ add(score,ruleI,null) }
		}
	}


	//-----
	// Chart
	//-----
	type RuleList = Array[BestList]
	type Chart = Array[Array[RuleList]]

	val (lexRules, scores, posScores)
			:(Array[(Rule,Int)],Array[Counter[Int]],Array[Counter[Int]]) = {
		val rules = RULES.zipWithIndex.filter( p => p._1.accepts(Head.Word) )
		val score = RULES.map( p => {
				new ClassicCounter[Int]
			}).toArray
		val pos = RULES.map( p => {
				new ClassicCounter[Int]
			}).toArray
		(rules, score, pos)
	}
	def lexCount(lex:Int,rule:Int):Double = scores(rule).getCount(lex)
	def lexProb(lex:Int,rule:Int):Double = {
		val count = lexCount(lex,rule)
		val denom = scores(rule).totalCount
		if(denom == 0.0) 0.0 else (count/denom)
	}
	def posCount(pos:Int,rule:Int):Double = posScores(rule).getCount(pos)
	def posProb(pos:Int,rule:Int):Double = {
		val count = posCount(pos,rule)
		val denom = posScores(rule).totalCount
		if(denom == 0.0) 0.0 else (count/denom)
	}
	
	def makeChart:Int=>Chart = { //start,length,split
		var largestChart = new Chart(0)
		(len:Int) => {
			//--Make Chart
			val chart = if(len > largestChart.length){ 
				//(create)
				largestChart = (0 until len).map{ (start:Int) =>          //begin
					assert(len-start > 0, "bad length end on start "+start+" len "+len)
					(0 until (len-start)).map{ (length:Int) =>              //length
						assert(Head.values.size > 0, "bad rules end")
						(0 until Head.values.size).map{ (ruleI:Int) =>        //rules
							assert(ckyK > 0, "bad kbest end")
							new BestList((0 until ckyK).map{ (kbestItem:Int) => //kbest
								new ChartElem
							}.toArray) //convert to arrays
						}.toArray
					}.toArray
				}.toArray
				//(return)
				largestChart
			} else {
				//(cached)
				largestChart
			}
			//--Reset Chart
			for(start <- 0 until len){
				for(len <- 0 until chart(start).length){
					for(head <- 0 until chart(start)(len).length){
						chart(start)(len)(head).reset
					}
				}
			}
			//--Return
			chart
		}
	}
	
	//-----
	// Access/Set
	//-----
	def gram(chart:Chart,begin:Int,end:Int,head:Int):BestList = {
		if(end == begin+1){ return lex(chart,begin,head) }
		//(asserts)
		assert(end > begin+1, "Chart access error: bad end: " + begin + ", " + end)
		assert(begin >= 0, "Chart access error: negative values: " + begin)
		assert(head >= 0, "Chart access error: bad head: " + head)
		assert(head < Head.values.size, "Chart access error: bad head: " + head)
		//(access)
		chart(begin)(end-begin-1)(head)
	}
	def lex(chart:Chart,elem:Int,head:Int):BestList = {
		//(asserts)
		assert(elem >= 0, "Chart access error: negative value: " + elem)
		assert(head >= 0, "Chart access error: bad head: " + head)
		assert(head < Head.values.size, "Chart access error: bad head: " + head)
		chart(elem)(0)(head)
	}
	def klex(sent:Sentence,elem:Int,y:(Int,Double)=>Boolean):Int = {
		val word = sent.words(elem)
		val pos = sent.pos(elem)
		//(get candidate parses)
		val candidates = lexRules.map( (pair:(Rule,Int)) => {
			val (r,rI) = pair
			(rI, lexProb(word,rI)) //TODO POS backoff
		})
		//(sort)
		candidates.sortBy( -_._2 )
		//(yield)
		for( i <- 0 until candidates.length) {
			val (rI,score) = candidates(i)
			if(!y(rI,score)){ return i; }
		}
		//(return)
		return candidates.length
	}

	//-----
	// CKY
	//-----
	def cky[T](sent:Sentence):Array[ParseTree] = {
		//--Create Chart
		val chart = makeChart(sent.length)
		assert(chart.length >= sent.length, "Chart is too small")
		//--Lex
		for(elem <- 0 until sent.length) {
			//(add terms)
			klex(sent,elem,(ruleI:Int,score:Double) => {
				val typeIndex = RULES(ruleI).head.id
				lex(chart,elem,typeIndex).suggest(score,ruleI)
				true
			})
			//(check)
			if(O.paranoid){
				var count:Int = 0
				Head.values.foreach{ head:Head.Value => 
					count += lex(chart,elem,head.id).length
				}
				assert(count > 0, "Word " + elem + " should have lex completions")
			}
		}
		//--Grammar
		for(begin <- 0 until sent.length-2) {         // begin
			for(length <- 2 until sent.length-begin) {  // length
				val end:Int = begin+length
				RULES_INDEX.foreach{ pair =>              // rules
					val (r,ruleI) = pair
					val head:Head.Value = r.head
					val headI:Int = head.id
					if(r.arity == 1){
						val child:BestList = gram(chart,begin,end,r.child.id)
						gram(chart,begin,end,headI).combine(ruleI,child,
							(left:ChartElem,right:ChartElem) => {
								0.0 //TODO
							})
					} else if(r.arity == 2){
						for(split <- (begin+1) until (end-1)){ // splits
							val left:BestList = gram(chart,begin,split,r.left.id)
							val right:BestList = gram(chart,split,end,r.right.id)
							gram(chart,begin,end,headI).combine(ruleI,left,right,
								(left:ChartElem,right:ChartElem) => {
									0.0 //TODO
								})
						}
					} else {
						throw new IllegalStateException("bad arity rule")
					}
				}
			}
		}
		//--Return
		gram(chart,0,sent.length,Head.ROOT.id).toArray.map{ x => x }
	}


	//-----
	// Parse Method
	//-----
	override def parse(i:Int, sent:Sentence, feedback:Boolean
			):(Array[Parse],Feedback=>Any)={
		log("|cky|=" + cky(sent).length + " for " + sent)
		val parse:Array[Parse] = Array[Parse](
			FRI(NOW),                              // I think it's friday
			(r:Range) => Range(r.begin,NOW),       // or 'the past'
			WEEK,                                  // or a week
			Range(Time(2011,4,26),Time(2011,4,27)) // or April 26
			)
		(parse, (feedback:Feedback) => {})
	}
}
