package time

import scala.collection.JavaConversions._

import Lex._
import Conversions._
import ParseConversions._

import org.goobs.exec.Log._
import org.goobs.exec.Execution

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
	override def toString:String = U.sent2str(words)
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
case class Tree[A](head:A,children:Array[Tree[A]]) {
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
object Head extends Enumeration {
	type V = Value
	val Time, Range, Duration, F_RR, F_RD, F_R, F_D, Word = Value
}
trait Rule {
	def apply(arg:Any):Any
	def apply(arg1:Any, arg2:Any):Any
	def arity:Int
	def outType:Head.Value
	def accepts(a:Head.Value):Boolean
	def accepts(a:Head.Value,b:Head.Value):Boolean
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
		outType.toString + "->" + children
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
	def outType:Head.Value = out
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
	def outType:Head.Value = out
	def accepts(a:Head.Value):Boolean = false
	def accepts(a:Head.Value,b:Head.Value):Boolean = (a == in1) && (b == in2)
	override def toString:String =
		""+out+"->"+in1+","+in2
}

object SearchParser {
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
//-----
// VALUES
//-----

	private val RULES:Array[Rule] = {
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

		//--Return
		rtn.toArray
	}


	private val NIL = -1 //LEX index of the NIL term
	private val RULES_INDEX = RULES.zipWithIndex
}


class SearchParser extends StandardParser {
	import SearchParser._
	import scala.math.{min,max,log => ln,exp}
//-----
// BEHAVIOR
//-----
	private val weights:Counter[Feature] = new ClassicCounter[Feature]

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
//			println(feats)
			weights.addAll(feats)
		} else {
			val negCounts:Counter[Feature] = new ClassicCounter[Feature]
			feats.keySet.foreach( (f:Feature) => {
				negCounts.incrementCount(f,-1.0*feats.getCount(f))
			})
			weights.addAll(negCounts)
//			print("NEG>>"); println(negCounts)
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
			parse._1 match{
				case Head.Time => {
					val t:Time = parse._2.asInstanceOf[Time]
					Parse(Range(t,t),null,null)
				}
				case Head.Range => Parse(parse._2.asInstanceOf[Range],null,null)
				case Head.Duration => Parse(null,parse._2.asInstanceOf[Duration],null)
				case Head.F_R => Parse(null,null,parse._2.asInstanceOf[Range=>Range])
				case _ => throw fail("Invalid end state: " + parse._1)
			}
		}

		override def isEndState = {
			if(begin <= 0 && end >= sent.length){
				parse._1 match{
					case Head.Time => true
					case Head.Range => true
					case Head.Duration => true
					case Head.F_R => true
					case _ => false
				}
			} else {
				false
			}
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
								(rule.outType, rule(sent(wordI))),
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
							leftOf=(rule.outType, rule(sent(begin-1))), 
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
							rightOf=(rule.outType, rule(sent(end+1))), 
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
								parse=(rule.outType,rule(parse._2)),
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
										parse=(rule.outType,rule(nodeValue, parseValue)),
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
										parse=(rule.outType,rule(parseValue, nodeValue)),
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
			):(Array[Parse],(Int,Boolean,Double)=>Any)={
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
		val update = (index:Int,exact:Boolean,score:Double) => {
			if(exact && index != 0){ //something is right, and not gotten yet
				val gold = parses(index)
				val guess = parses(0)
				gold.updates.foreach( _(true,score) )
				guess.updates.foreach( _(false,score) )
			}
		}
		//--Debug
		log("Parsed \"" + U.sent2str(sent.words) + "\" ("+parses.length+") as " + 
			U.join(
				parses.slice(0,1).map(
					p => ""+p.realParse+"["+G.df.format(p.cost)+"]"), " or "))
		(parses.map( _.realParse ), if(feedback){ update } else { (i,e,s)=>{} } )
	}
}


