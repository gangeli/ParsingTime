package time

import scala.collection.JavaConversions._
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashSet

import Lex._
import ParseConversions._

import org.goobs.exec.Log._
import org.goobs.exec.Execution
import org.goobs.slib.Def
import org.goobs.slib.JavaNLP._

import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.TwoDimensionalCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.stats.Counters;
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.sequences.SeqClassifierFlags
import edu.stanford.nlp.sequences.FeatureFactory

//------------------------------------------------------------------------------
// GRAMMAR
//------------------------------------------------------------------------------
object Head extends Enumeration {
	type V = Value
	val ROOT, Word, Number, 
		Time, Range, Duration, Sequence, F_RR, F_RD, F_R, F_D, NIL = Value
}
trait Rule {
	// -- Must Override --
	def apply(arg:Any):Any
	def apply(arg1:Any, arg2:Any):Any
	def arity:Int
	def head:Head.Value
	def accepts(a:Head.Value):Boolean
	def accepts(a:Head.Value,b:Head.Value):Boolean
	
	// -- Can Override --
	def validInput(w:Int) = isLex
	def setStr(str:String) = {}

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
	def child:Head.Value = {
		assert(arity == 1, "Bad arity"); cacheRule; return leftChild;
	}
	def isLex:Boolean = 
		(arity == 1 && (child == Head.Word || child == Head.Number))

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
	private var checkValid:Int=>Boolean = (i:Int) => true
	private var str:String = null; override def setStr(str:String) = this.str=str;
	override def apply(arg:Any):Any = fn(arg)
	override def apply(arg1:Any,arg2:Any) 
		= throw fail("binary apply to unary rule")
	override def validInput(w:Int):Boolean = isLex && checkValid(w)
	def arity:Int = 1
	def head:Head.Value = out
	def accepts(a:Head.Value):Boolean = a == in
	def accepts(a:Head.Value,b:Head.Value):Boolean = false
	def ensureValidity(fn:Int=>Boolean):UnaryRule = { checkValid = fn; this }
	override def toString:String = if(str != null){
			str
		} else {
			""+out+{if(in==Head.Word) "["+this(0)+"]" else ""}+"->"+in
		}
}
case class BinaryRule(
		out:Head.Value,
		in1:Head.Value,
		in2:Head.Value,
		fn:(Any,Any)=>Any) extends Rule {
	private var str:String = null; override def setStr(str:String) = this.str=str;
	override def apply(arg:Any)
		= throw fail("unary apply to binary rule")
	override def apply(arg1:Any,arg2:Any):Any = fn(arg1,arg2)
	def arity:Int = 2
	def head:Head.Value = out
	def accepts(a:Head.Value):Boolean = false
	def accepts(a:Head.Value,b:Head.Value):Boolean = (a == in1) && (b == in2)
	override def toString:String = if(str != null){
			str
		} else {
			""+out+"->"+in1+","+in2
		}
}

object Grammar {
	case class NIL()

	val DOW_STR = Array[String]("Mon:D","Tue:D","Wed:D","Thu:D","Fri:D",
		"Sat:D","Sun:D")
	val MOY_STR = Array[String]("Jan:D","Feb:D","Mar:D","Apr:D","May:D",
		"Jun:D","Jul:D","Aug:D","Sep:D","Oct:D","Nov:D","Dec:D")
	val QOY_STR = Array[String]("Q1:D","Q2:D","Q3:D","Q4:D")
	
	private val NAMED_RULES:Array[(Rule,String)] = {
		def hack[A,Z](fn:A=>Z):Any=>Any = fn.asInstanceOf[Any=>Any]
		def hack2[A,B,Z](fn:(A,B)=>Z):(Any,Any)=>Any 
			= fn.asInstanceOf[(Any,Any)=>Any]
		var rtn = List[(Rule,String)]()
		//--Lex
		//(times)
		val times = List[(Range,String)]((REF,"REF:T"))
		rtn = rtn ::: times.map{ case (t:Range,s:String) => 
			(UnaryRule(Head.Time, Head.Word, hack((w:Int) => t)), s) }
		//(ranges)
		val ranges = List[(Range,String)]((TODAY,"REF:R"))
		rtn = rtn ::: ranges.map{ case (r:Range,s:String) => 
			(UnaryRule(Head.Range, Head.Word, hack((w:Int) => r)), s) }
		//(durations)
		val durations = 
			{if(O.useTime) List[(Duration,String)]((SEC,"Sec:D"),(MIN,"Min:D"),
				(HOUR,"Hour:D")) else List[(Duration,String)]()} :::
			List[(Duration,String)](
				(DAY,"Day:D"),(WEEK,"Week:D"),(MONTH,"Month:D"),(QUARTER,"Quarter:D"),
				(AYEAR,"Year:D"))
		val sequences = 
			(1 to 7).map( i => (DOW(i),DOW_STR(i-1)) ).toList :::
			(1 to 12).map( i => (MOY(i),MOY_STR(i-1)) ).toList :::
			(1 to 4).map( i => (QOY(i),QOY_STR(i-1)) ).toList
		rtn = rtn ::: durations.map{ case (d:Duration,s:String) => 
			(UnaryRule(Head.Duration, Head.Word, hack((w:Int) => d)), s) }
		rtn = rtn ::: sequences.map{ case (d:Duration,s:String) => 
			(UnaryRule(Head.Sequence, Head.Word, hack((w:Int) => d)), s) }
		//(nil)
		rtn = rtn ::: List[(Rule,String)](
			(UnaryRule(Head.NIL, Head.Word, hack((w:Int) => new NIL)), "nil") )
		//(numbers)
		rtn = rtn ::: List[(Rule,String)](
			(UnaryRule(Head.Number, Head.Number, hack((num:Int) =>  num )),
				"NUM"),
			(UnaryRule(Head.Sequence, Head.Number, hack((num:Int) =>  DOW(num) ))
				.ensureValidity( (w:Int) => w >= 1 && w <= 7 ),
				"dow(n):D"),
			(UnaryRule(Head.Sequence, Head.Number, hack((num:Int) =>  DOM(num) ))
				.ensureValidity( (w:Int) => w >= 1 && w <= 31 ),
				"dom(n):D"),
			(UnaryRule(Head.Sequence, Head.Number, hack((num:Int) =>  WOY(num) ))
				.ensureValidity( (w:Int) => w >= 1 && w <= 52 ),
				"woy(n):D"),
			(UnaryRule(Head.Sequence, Head.Number, hack((num:Int) =>  MOY(num) ))
				.ensureValidity( (w:Int) => w >= 1 && w <= 12 ),
				"moy(n):D"),
			(UnaryRule(Head.Sequence, Head.Number, hack((num:Int) =>  QOY(num) ))
				.ensureValidity( (w:Int) => w >= 1 && w <= 4 ),
				"qoy(n):D"),
			(UnaryRule(Head.Sequence, Head.Number, hack((num:Int) =>  YOC(num) ))
				.ensureValidity( (w:Int) => w >= 0 && w <= 99 ),
				"yoc(n):D"),
			(UnaryRule(Head.Range, Head.Number, hack((num:Int) =>  THEYEAR(num) )),
				"year(n):R"),
			(UnaryRule(Head.Range, Head.Number, hack((num:Int) =>  DECADE(num) )),
				"decade(n):R"),
			(UnaryRule(Head.Range, Head.Number, hack((num:Int) =>  CENTURY(num) )),
				"century(n):R")
			)

		//--F[ Range, Duration ]
		val rangeDurationFn = List[((Range,Duration)=>Range,String,Boolean)](
			(shiftLeft,"shiftLeft",true),(shiftRight,"shiftRight",true),
			(catLeft,"catLeft",false),(catRight,"catRight",false),
			(shrinkBegin,"shrinkBegin",false),(shrinkEnd,"shrinkEnd",false) )
		rtn = rtn ::: rangeDurationFn.foldLeft(List[(Rule,String)]()){ case 
					(soFar:List[(Rule,String)],
						(fn:((Range,Duration)=>Range),
						 str:String,
						 seqAlso:Boolean)) =>
				//(intro)
				(UnaryRule(Head.F_RD, Head.Word, hack((w:Int) => fn
					)),str+"$(-:R,-:D):R$") :: soFar
			}  ::: {
				//(right apply)
				(BinaryRule(Head.F_R, Head.F_RD, Head.Duration, hack2(
					(fn:(Range,Duration)=>Range,d:Duration) => fn(_:Range,d)
					)),"$f(-:R,d:D):R$") ::
				(BinaryRule(Head.F_R, Head.F_RD, Head.Sequence, hack2(
					(fn:(Range,Duration)=>Range,d:Duration) => fn(_:Range,d)
					)),"$f(-:R,d:S):R$") ::
				//(left apply)
				(BinaryRule(Head.F_R, Head.Duration, Head.F_RD, hack2(
					(d:Duration,fn:(Range,Duration)=>Range) => fn(_:Range,d)
					)), "$f(-:R,d:D):R$") :: 
				(BinaryRule(Head.F_R, Head.Sequence, Head.F_RD, hack2(
					(d:Duration,fn:(Range,Duration)=>Range) => fn(_:Range,d)
					)), "$f(-:R,d:S):R$") :: Nil
			}
		
		//--F[ Range, Range ]
		val rangeRangeFn = List[((Range,Range)=>Range,String)](
			(intersect,"intersect"),(cons,"cons"))
		rtn = rtn ::: rangeRangeFn.foldLeft(List[(Rule,String)]()){ case 
					(soFar:List[(Rule,String)],
						(fn:((Range,Range)=>Range),
						 str:String)) =>
				//(intro)
				(UnaryRule(Head.F_RR, Head.Word, hack((w:Int) => fn
					)),str+"$(-:R,-:R):R$") :: soFar
			} ::: {
				//(right apply)
				(BinaryRule(Head.F_R, Head.F_RR, Head.Range, hack2(
					(fn:(Range,Range)=>Range,r:Range) => fn(_:Range,r)
					)),"$f(-:R,r:R):R$") ::
				//(left apply)
				(BinaryRule(Head.F_R, Head.Range, Head.F_RR, hack2(
					(r:Range,fn:(Range,Range)=>Range) => fn(_:Range,r)
					)), "$f(-:R,r:R):R$") :: Nil
			}
		
		//--F[ Duration, Number ]
		rtn = rtn ::: List[(Rule,String)](
			(BinaryRule(Head.Duration, Head.Duration, Head.Number, hack2(
				(d:Duration,n:Int) => d*n
				)), "D*n"),
			(BinaryRule(Head.Duration, Head.Number, Head.Duration, hack2(
				(n:Int,d:Duration) => d*n
				)), "n*D")
			)
		
		//--F[ Range ]
		rtn = rtn ::: List[(Rule,String)](
			//(right apply)
			(BinaryRule(Head.Range, Head.F_R, Head.Range, hack2(
				(fn:Range=>Range,r:Range) => fn(r)
				)), "$f_{r,r}:R$"),
			//(left apply)
			(BinaryRule(Head.Range, Head.Range, Head.F_R, hack2(
				(r:Range,fn:Range=>Range) => fn(r)
				)), "$f_{r,r}:R$")
			)
		
		//--F[ Duration ]
		rtn = rtn ::: List[(Rule,String)](
			//(right apply)
			(BinaryRule(Head.Range, Head.F_D, Head.Duration, hack2(
				(fn:Duration=>Range,d:Duration) => fn(d)
				)), "$f_{d}:R$"),
			(BinaryRule(Head.Range, Head.F_D, Head.Sequence, hack2(
				(fn:Duration=>Range,d:Duration) => fn(d)
				)), "$f_{s}:R$"),
			//(left apply)
			(BinaryRule(Head.Range, Head.Duration, Head.F_D, hack2(
				(d:Duration,fn:Duration=>Range) => fn(d)
				)), "$f_{d}:R$"),
			(BinaryRule(Head.Range, Head.Sequence, Head.F_D, hack2(
				(d:Duration,fn:Duration=>Range) => fn(d)
				)), "$f_{s}:R$")
			)
		
		//--Type Raises
		rtn = rtn ::: List[(Rule,String)]( 
			//(now augmentation (arity 2))
			(UnaryRule(Head.F_D, Head.F_RD, hack( 
				(f:(Range,Duration)=>Range) => f(Range(REF,REF),_:Duration) 
				)),"$f(ref:R,-:D):R$"),
			(UnaryRule(Head.F_R, Head.F_RR, hack( 
				(f:(Range,Range)=>Range) => f(Range(REF,REF),_:Range) 
				)),"$f(ref:R,-:R):R$"),
			(UnaryRule(Head.F_R, Head.F_RR, hack( 
				(f:(Range,Range)=>Range) => f(_:Range,Range(REF,REF)) 
				)),"$f(-:R,ref:R):R$"),
			//(implicit intersect)
			(UnaryRule(Head.F_R, Head.Range, hack(
				(r:Range) => intersect(r,_:Range)
				)),"intersect$(ref:R,-:R):R$"),
			//(sequence grounding)
			(UnaryRule(Head.Range, Head.Sequence, hack( 
				(s:Sequence) => s
				)),"$d:R$")
			)

		//--NIL Identities
		rtn = rtn ::: List[(Rule,String)]( 
			//(range)
			(BinaryRule(Head.Range, Head.Range, Head.NIL, hack2( 
				(r:Range,n:NIL) => r
				)),"$r:R$"),
			(BinaryRule(Head.Range, Head.NIL, Head.Range, hack2( 
				(n:NIL,r:Range) => r
				)),"$r:R$"),
			//(duration)
			(BinaryRule(Head.Duration, Head.Duration, Head.NIL, hack2( 
				(d:Duration,n:NIL) => d
				)),"$d:D$"),
			(BinaryRule(Head.Duration, Head.NIL, Head.Duration, hack2( 
				(n:NIL,d:Duration) => d
				)),"$d:D$"),
			//(sequence)
			(BinaryRule(Head.Sequence, Head.Sequence, Head.NIL, hack2( 
				(d:Duration,n:NIL) => d
				)),"$s:D$"),
			(BinaryRule(Head.Sequence, Head.NIL, Head.Sequence, hack2( 
				(n:NIL,d:Duration) => d
				)),"$s:D$"),
			//(f_range)
			(BinaryRule(Head.F_R, Head.F_R, Head.NIL, hack2( 
				(f:Range=>Range,n:NIL) => f
				)),"$f(-:R):R$"),
			(BinaryRule(Head.F_R, Head.NIL, Head.F_R, hack2( 
				(n:NIL,f:Range=>Range) => f
				)),"$f(-:R):R$"),
			//(f_duration)
			(BinaryRule(Head.F_D, Head.F_D, Head.NIL, hack2( 
				(f:Duration=>Range,n:NIL) => f
				)),"$f(-:D):R$"),
			(BinaryRule(Head.F_D, Head.NIL, Head.F_D, hack2( 
				(n:NIL,f:Duration=>Range) => f
				)),"$f(-:D):R$")
			)

		//--Return
		rtn.foreach{ case (r:Rule,s:String) => r.setStr(s) }
		rtn.toArray
	}
	
	val RULES:Array[Rule] = {
		def hack[A,Z](fn:A=>Z):Any=>Any = fn.asInstanceOf[Any=>Any]
		def hack2[A,B,Z](fn:(A,B)=>Z):(Any,Any)=>Any 
			= fn.asInstanceOf[(Any,Any)=>Any]
		var rtn = List[Rule]()
		
		//--Named Rules
		rtn = rtn ::: NAMED_RULES.map( _._1 ).toList

		//-ROOT
		rtn = rtn ::: List[UnaryRule](
			UnaryRule(Head.ROOT, Head.Time, hack((t:Range) => t)),
			UnaryRule(Head.ROOT, Head.Range, hack((r:Range) => r)),
			UnaryRule(Head.ROOT, Head.Duration, hack((d:Duration) => d)),
			UnaryRule(Head.ROOT, Head.F_R, hack((fn:Range=>Range) => fn))
			)
		//--Return
		rtn.toArray
	}

	val NIL_RID:Int = {
		val matches:Array[(Rule,Int)] 
			= RULES.zipWithIndex.filter{ _._1.head == Head.NIL }
		assert(matches.length == 1, "invalid nil rule count (should be 1)")
		matches(0)._2
	}

	val UNARIES:Array[(Rule,Int)]  = RULES.zipWithIndex.filter{ _._1.arity == 1 }
	val BINARIES:Array[(Rule,Int)] = RULES.zipWithIndex.filter{ _._1.arity == 2 }
	val RULES_INDEX = RULES.zipWithIndex

	val RULES_STR:Array[String] = {
		val rtn = RULES.map{ _.head.toString }
		for(i <- 0 until NAMED_RULES.length){
			assert(RULES(i) == NAMED_RULES(i)._1, "name mismatch")
			rtn(i) = NAMED_RULES(i)._2
		}
		rtn
	}

	case class Closure(head:Head.Value,child:Head.Value,rules:Array[Int])

	private def computeClosures(raw:Array[Rule]):Array[Closure] = {
		//--Construct Graph
		case class Node(head:Head.Value,var neighbors:List[(Node,Int)]){
			def this(head:Head.Value) = this(head,List[(Node,Int)]())
			def addNeighbor(n:Node,rid:Int) = { neighbors = (n,rid)::neighbors }
			def search(seen:Array[Boolean],backtrace:List[Int],
					tick:(Head.Value,List[Int])=>Any):Unit = {
				//(overhead)
				if(seen(head.id)){ 
					throw new IllegalStateException("Cyclic unaries for: " + head)
				}
				seen(head.id) = true
				//(report path)
				if(backtrace.length > 0){ tick(head,backtrace) }
				//(continue searching
				neighbors.foreach{ case (node,rid) =>
					assert(RULES(rid).head == this.head, "graph constructed badly (head)")
					assert(RULES(rid).child == node.head, "graph constructed badly (ch)")
					node.search(seen,rid :: backtrace,tick)
				}
				//(pop up)
				seen(head.id) = false
			}
		}
		//(populate graph)
		val graph = Head.values.toArray.map{ new Node(_) }
		UNARIES.foreach{ case (r,rid) => 
			assert(r.head != Head.Word, "Unary headed by a Word")
			if(r.child != Head.Word && r.child != Head.Number){ //don't add lex rules
				graph(r.head.id).addNeighbor(graph(r.child.id),rid) 
			}
		}
		//--Search Graph
		var closures = List[Closure]()
		graph.foreach{ (start:Node) => 
			start.search(new Array[Boolean](graph.length), List[Int](),
				(child:Head.Value,backtrace:List[Int]) => {
					//(format backtrace)
					val rules:Array[Int] = backtrace.reverse.toArray
					assert(RULES(rules(0)).head == start.head, "bad head")
					assert(RULES(rules(rules.length-1)).child == child, "bad child")
					//(add closure)
					closures = Closure(start.head,child,rules) :: closures
				})
		}
		closures.toArray
	}

	val CLOSURES:Array[Closure] = computeClosures(RULES)
	val CLOSURES_INDEX:Array[(Closure,Int)] = CLOSURES.zipWithIndex

	val str2rid:scala.collection.immutable.Map[String,Int]
		= RULES_STR.zipWithIndex.toMap
	
}

//------------------------------------------------------------------------------
// PARSER
//------------------------------------------------------------------------------
//-----
// Utilities
//-----
object ParseConversions {
	implicit def time2parse(t:Time):Parse = Parse(Range(t,t))
	implicit def range2parse(r:Range):Parse = Parse(r)
	implicit def duration2parse(d:Duration):Parse = Parse(d)
	implicit def fn2parse(fn:Range=>Range):Parse = Parse(new PartialTime(fn))
}

//-----
// Input / Output
//-----
case class Sentence(id:Int,words:Array[Int],pos:Array[Int],nums:Array[Int]) {
	def apply(i:Int):String = {
		if(words(i) == G.NUM){
			nums(i).toString
		} else {
			U.w2str(words(i))
		}
	}
	def foreach(fn:(Int,Int)=>Any) = words.zip(pos).foreach(Function.tupled(fn))
	def length:Int = words.length
	override def toString:String = U.sent2str(words)
}
case class Feedback(ref:Temporal,grounding:Time,
		correct:Array[(Int,Double)],incorrect:Array[(Int,Double)]) {
	def bestScore = correct(0)._2
	def hasCorrect:Boolean = correct.length > 0
	def bestIndex:Int = correct(0)._1
	def tiedBest:Array[Int] = {
		correct.filter{ case (index,score) => score == bestScore }.map{ _._1 }
	}
	def isCorrect:Boolean = hasCorrect &&  bestIndex == 0
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
	//<<Common Methods>>
	private def cleanParseString(
			indent:Int,b:StringBuilder,sent:Sentence,i:Int):Int = {
		for(i <- 0 until indent){ b.append("  ") }
		if(isLeaf){
			//--Case: Leaf
			val head = headString
			val tail = leafString(sent,i)
			if(tail != null && !tail.equals("")){
				b.append("(").append(head.replaceAll(" ","_")).append(" ")
				b.append(tail.replaceAll(" ","_")).append(")")
			} else {
				b.append(head.replaceAll(" ","_"))
			}
			i+1
		} else {
			//--Case: Not Leaf
			//(overhead)
			var retI = i
			val ch = children
			//(write)
			b.append("(").append(headString.replaceAll(" ","_"))
			for(i <- 0 until ch.length) {
				val child:ParseTree = ch(i)
				b.append("\n")
				retI = child.cleanParseString(indent+1,b,sent,retI)
			}
			b.append(")")
			retI
		}
	}
	def asParseString(sent:Sentence):String = {
		val b = new StringBuilder
		cleanParseString(0,b,sent,0)
		b.toString
	}
	def lexRules:Array[Int] = {
		var terms = List[Int]()
		traverse( (rid:Int) => {}, (rid:Int,w:Int) => { terms = rid :: terms } )
		terms.reverse.toArray
	}
	//<<Possible Overrides>>
	def headString:String = head.toString
	def leafString(sent:Sentence,index:Int):String = sent(index)
	def maxDepth:Int = throw fail() //TODO implement something reasonable here
	//<<Overrides>>
	override def children:Array[ParseTree]
	def evaluate(sent:Sentence):(Head.Value,Temporal,Double)
	def traverse(ruleFn:Int=>Any,lexFn:(Int,Int)=>Any):Unit //lexFn: (rid,w)=>Any
}

//-----
// Parse
//-----
case class Parse(value:Temporal){
	def scoreFrom(gold:Temporal,ground:Time
			):Iterator[((Duration,Duration),Double,Int)]={
		val INF = (Duration.INFINITE,Duration.INFINITE)
		def diff(gold:Temporal,guess:Temporal,second:Boolean):(Duration,Duration) 
				= (gold,guess) match {
			//--Immediate Invalids
			//(case: unks)
			case (gold:UnkTime,guess:Temporal) => INF
			case (gold:Temporal,guess:NoTime) => INF
			//--Valid
			//(case: durations)
			case (gold:Duration,guess:Duration) => (guess-gold,Duration.ZERO)
			//(case: grounded ranges)
			case (gold:GroundedRange,guess:GroundedRange) => 
				if(O.instantAsDay && gold.norm.seconds == 0 && guess.norm.seconds != 0){
					(guess.begin-gold.begin,guess.end-(gold.end+DAY))
				} else {
					(guess.begin-gold.begin,guess.end-gold.end)
				}
			//--Possibly Valid
			//(case: backoffs)
			case (gold:Range,guess:GroundedRange) => 
				if(second){ INF}
				else { diff(gold(ground),guess,true) }
			case (gold:PartialTime,guess:Temporal) =>
				if(second){ INF}
				else { diff(gold(ground),guess,true) }
			//--Type Problems
			//(case: types)
			case (gold:Duration,guess:Temporal) =>  INF
			case (gold:Range,guess:Temporal) => INF
			//(case: default)
			case _ => throw fail("Unk (invalid?) case: gold "+gold+" guess "+guess)
		}
		//--Map Iterator
		value.distribution(ground).iterator.map{
				case (guess:Temporal,score:Double,i:Int) =>
			(diff(gold,guess,false),score,i)
		}
	}

	def ground(ground:Time):Temporal
		= if(value.exists(0)(ground)) value(0)(ground) else new NoTime
}

object Parse {
	def apply(parseType:Head.Value,parseValue:Temporal):Parse = {
		assert(parseType == Head.ROOT, "No parse for non-root node")
		new Parse(parseValue)
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
	def beginIteration(iter:Int,feedback:Boolean,data:DataStore):Unit = {}
	def endIteration(iter:Int,feedback:Boolean,data:DataStore):Unit = {}
	def parse(iter:Int, sent:Sentence, feedback:Boolean,sid:Int
		):(Array[Parse],Feedback=>Any)
	override def cycle(data:DataStore,iters:Int,feedback:Boolean):Array[Score] = {
		(1 to iters).map( (i:Int) => {
			start_track("Iteration " + i)
			beginIteration(i,feedback,data)
			val score = data.eachExample{ (sent:Sentence,id:Int) => 
				parse(i, sent, feedback, id)
			}
			endIteration(i,feedback,data)
			log("Score: " + score)
			end_track
			score
		}).toArray
	}
}
//------------------------------------------------------------------------------
// CKY PARSER
//------------------------------------------------------------------------------
class NeighboringWords extends FeatureFactory[CoreMap] {
	import edu.stanford.nlp.util.PaddedList
	import edu.stanford.nlp.sequences.Clique
	override def getCliqueFeatures(
			info:PaddedList[CoreMap],
			position:Int,
			clique:Clique):java.util.Collection[String] = {
		val wds:Array[String] = info
		val lastWord = if(position<=0) "^" else wds(position-1)
		val nextWord = if(position>=(info.size-1)) "$" else wds(position+1)
		List[String](
			""+lastWord+" <- "+wds(position)+" -> "+nextWord,
			""+lastWord+" <- "+wds(position),
			""+wds(position)+" -> "+nextWord,
			wds(position)
			)
	}
}

object CKYParser {
	val UNARY:Int = 0
	val BINARY:Int = 1
	import Grammar._

	//-----
	// CRF Tagger
	//-----
	object CRFTagger {
		private def debugSequence(data:Array[(Sentence,Array[Int])],
				ruleStr:Boolean=false):Unit = {
			import org.goobs.utils.Indexer;
			//(vars)
			val toS:Int=>String = if(ruleStr) RULES_STR(_) else U.w2str(_)
			val tagger = apply(data,false)
			//(group by sentence)
			val grouped:Array[(Sentence,Array[Array[Int]])] = {
				val indexer = new Indexer[Sentence]
				data.map(_._1).foreach{(sent:Sentence) => indexer.addAndGetIndex(sent)}
				(0 until indexer.size).map{ (i:Int) =>
					(indexer.get(i),
						data.filter{case (s:Sentence,t:Array[Int]) => indexer.indexOf(s)==i}
							.map(_._2).toArray)
				}.toArray
			}
			//(print info)
			grouped.foreach{ case (sent:Sentence,tags:Array[Array[Int]]) =>
				val guess = tagger.tag(sent)
				println("" + sent + "::  " + U.join(guess.map( toS(_) ), " " ))
				tags.foreach{ (gold:Array[Int]) =>
					if(guess.zip(gold).forall{ case(a:Int,b:Int) => a == b }){
						print(" *")
					} else {
						print("  ")
					}
					println(U.join(gold.map( toS(_) )," "))
				}
			}
		}
		def debugSequence:Unit = {
			debugSequence(Const.CRF_DATA_NOAMBIGUITY)
		}

		def apply(dataset:Array[(Sentence,Array[Int])],db:Boolean=false):CRFTagger={
			if(db){
				println("-----DEBUG SEQUENCE------")
				debugSequence(dataset,true)
				println("-------------------------")
			}
			start_track("Training CRF Classifier")
			//(create data)
			log("creating data...")
			val javaData:java.util.List[java.util.List[CoreMap]] =
				dataset.map{ case (sent:Sentence,ann:Array[Int]) => 
					assert(ann.forall{ _.toString.toInt >= 0}, "numbers crashed")
					annotate( 
						sent2coremaps(
							sent.words.map(U.w2str(_)),sent.pos.map(U.pos2str(_))),
						ann.map{ _.toString} )
				}.toList
			//(create flags)
			val flags = new SeqClassifierFlags
			flags.featureFactory = O.crfFeatureFactory
			debug("flags.featureFactory: " + flags.featureFactory)
			flags.backgroundSymbol = NIL_RID.toString
			debug("flags.backgroundSymbol: " + flags.backgroundSymbol)
			flags.inferenceType = "Beam"
			debug("flags.inferenceType: " + flags.inferenceType)
			flags.beamSize = O.crfKBest
			debug("flags.beamSize: " + flags.beamSize)
			val classifier = new CRFClassifier[CoreMap](flags)
			//(train classifier)
			log("training...")
			//(debug) TODO removeme
//			javaData.foreach{ (jlst:java.util.List[CoreMap]) => 
//				val lst = jlst
//				import edu.stanford.nlp.ling.CoreAnnotations.AnswerAnnotation
//				import edu.stanford.nlp.ling.CoreAnnotations.TextAnnotation
//				val strs:Array[String] = lst.map{ (m:CoreMap) => 
//					""+m.get[String,TextAnnotation](WORD)+
//					"("+RULES_STR(m.get[String,AnswerAnnotation](ANSWER).toInt)+")"
//				}.toArray
//				println("train:  " + U.join(strs, "  ") ) 
//			}
			classifier.train(javaData)
			end_track
			new CRFTagger(classifier)
		}
	}
	class CRFTagger(classifier:CRFClassifier[CoreMap]) {
		import edu.stanford.nlp.ling.CoreAnnotations.AnswerAnnotation
		import edu.stanford.nlp.ling.CoreAnnotations.TextAnnotation
		def tag(sent:Sentence):Array[Int] = {
			val tagged = classifier.classify(
				sent2coremaps(sent.words.map(U.w2str(_)),sent.pos.map(U.pos2str(_))) )
			tagged.map{ (term:CoreMap) => 
				term.get[String,AnswerAnnotation](ANSWER).toInt
			}.toArray
		}
		def tagK(sent:Sentence,k:Int):Array[(Array[Int],Double)] = {
			val counter = classifier.classifyKBest(
				sent2coremaps(sent.words.map(U.w2str(_)),sent.pos.map(U.pos2str(_))),
				ANSWER,
				k)
			counter.keySet.map{ (tagged:java.util.List[CoreMap]) =>
				(
					tagged.map{ (term:CoreMap) => 
						term.get[String,AnswerAnnotation](ANSWER).toInt
					}.toArray,
					U.sigmoid(counter.getCount(tagged))
				)
			}.toArray.sortBy( _._2 )
		}
		def klex(sent:Sentence,k:Int,y:(CkyRule,Int,Double)=>Boolean):Array[Int] = {
			//(tag)
			val sequences = tagK(sent,k)
			val sum = sequences.foldLeft(0.0) 
				{ case (sofar:Double,(term:Array[Int],score:Double)) => sofar + score }
			assert(sequences.forall( _._2 >= 0.0), "Negative score")
			//(debug) //TODO removeme
//			println("-----KLEX " + sent + "------")
//			sequences.foreach{ case (term:Array[Int],score:Double) => 
//				println(sent + " (" + G.df.format(score) + ") :: " + U.join(term.map{ (i:Int) => CKY_LEX(rid2lexI(i))},"   "))
//			}
			//( P(slot,rule) )
			val pSlotRule:Array[Array[Double]] = {
				//(create)
				val probs:Array[Array[Double]] = (0 until sent.length).map{ (i:Int) =>
					(0 until CKY_LEX.length).map{ (rid:Int) => 0.0 }.toArray
				}.toArray
				//(fill)
				sequences.foreach{ case (rids:Array[Int],score:Double) =>
					rids.zipWithIndex.foreach{ case (rid:Int,i:Int) => 
						assert(rid2lexI(rid) >= 0, "Invalid rule " + rid)
						if(sum != 0.0){
							probs(i)(rid2lexI(rid)) = (score / sum)
						} else {
							probs(i)(rid2lexI(rid)) = 1.0/CKY_LEX.length.asInstanceOf[Double]
						}
					}
				}
				//(return)
				probs
			}
			//(yield)
			(0 until sent.length).map{ (i:Int) =>
				var shouldCont:Boolean = true
				var count:Int = 0
				pSlotRule(i).zipWithIndex.sortBy(-_._1)
						.foreach{ case (p:Double,lexI:Int) =>
					assert(p >= 0.0, "Negative probability")
					if(p == 0.0){ shouldCont = false }
					if(shouldCont){
						shouldCont = y(CKY_LEX(lexI),i,U.safeLn(p))
						count += 1
					}
				}
				count
			}.toArray
		}
	}
	
	//-----
	// Values
	//-----
	//(guessed parses)
	private case class GuessInfo
			(sid:Int,parse:ParseTree,score:Double,sent:Sentence){
		var correct=true; def wrong:GuessInfo = {correct = false; this}
		var feedback:Feedback=null
		def feedback(f:Feedback):GuessInfo = { feedback=f; this }
		def parseVal:Parse = Parse(Head.ROOT,parse.evaluate(sent)._2)
		def isFailedParse:Boolean = { parse == null }
	}
	private var corrects = List[GuessInfo]()
	private var guesses = List[GuessInfo]()

	//(rules)
	val CKY_UNARY:Array[CkyRule] = CLOSURES.map{ (closure:Closure) =>
			CkyRule(1,closure.head,closure.child,closure.rules)
		}
	val CKY_LEX:Array[CkyRule] = UNARIES
		.filter{ case (rule,rid) => rule.isLex }
		.map{ case (rule,rid) =>
			assert(rule.arity == 1, "unary rule is not unary")	
			CkyRule(rule.arity,rule.head,rule.child,Array[Int](rid))
		}
	val CKY_BINARY:Array[CkyRule] = BINARIES.map{ case (rule,rid) => 
			assert(rule.arity == 2, "binary rules computed wrong")
			CkyRule(rule.arity,rule.head,null,Array[Int](rid))
		}
	//(utilities)
	val rid2lexI:Array[Int] = (0 until RULES.length).map{ (rid:Int) =>
			val matches = 
				CKY_LEX.zipWithIndex.filter{ case (r:CkyRule,i:Int) => r.rid == rid }
			assert(matches.length <= 1, "multiple cky rules for rid " + rid)
			if(matches.length > 0) matches(0)._2 else -1
		}.toArray
	
	//(counts)
	private val rulesCounted:Array[Double] = RULES.map{ r => 0.0 }
	private val wordsCounted:Array[Counter[Int]] = 
		RULES.map{ r => new ClassicCounter[Int] }
	
	//(parameters)
	private val ruleScores:Array[Double] = (0 until RULES.length).map{(rid:Int) =>
		if(RULES(rid).isLex){
			Double.NaN
		} else {
			O.initMethod match {
				case O.InitType.uniform => 1.0
				case O.InitType.random => U.rand
			}
		}
	}.toArray
	private val wordScores:Array[Array[Double]] = (0 until RULES.length).map{ r =>
		(0 to G.W).map{ w =>
			O.initMethod match {
				case O.InitType.uniform => 1.0
				case O.InitType.random => U.rand
			}
		}.toArray}.toArray
	private val wordTotalCounts:Array[Double] = 
		(0 until RULES.length).map{ r => 0.0 }.toArray
	
	//(substructures)
	private var tagger:CRFTagger = null
	
	

	//-----
	// Subclasses
	//-----
	case class CkyRule(
			arity:Int,
			head:Head.Value,
			childOrNull:Head.Value,
			rids:Array[Int]) {
		def rid:Int = {
			if(rids.length > 1){ 
				throw new IllegalArgumentException("multiple rules")
			}
			rids(0)
		}
		def rule:Rule = (RULES(rid))
		def rules:Array[Rule] = rids.map{ RULES(_) }
		def child:Head.Value = {
			assert(arity == 1, "child only defined for unary rules")
			assert(childOrNull != null, "something went very wrong")
			childOrNull
		}
		def numChildren:Int = rids.length
		def isLex:Boolean = rids.length == 1 && rule.isLex
		def validInput(w:Int):Boolean = rule.validInput(w)
		override def toString:String = "<" + U.join(rules,", ") + ">"
	}

	class ChartElem(
			var logScore:Double, 
			var term:CkyRule, 
			var left:ChartElem,
			var right:ChartElem) extends ParseTree {
		// -- CKY Properties --
		def this() = this(Double.NegativeInfinity,null,null,null)
		def this(logScore:Double,term:CkyRule) = this(logScore,term,null,null)
		def apply(logScore:Double,term:CkyRule,left:ChartElem,right:ChartElem
				):ChartElem = {
			this.logScore = logScore
			this.term = term
			this.left = left
			this.right = right
			this
		}
		def apply(logScore:Double,term:CkyRule,left:ChartElem):ChartElem = {
			assert(term.arity == 1, "Invalid apply for arity 1 rule")
			apply(logScore,term,left,null)
		}
		def apply(other:ChartElem):ChartElem = {
			assert(!other.isNil, "Setting to nil chart element")
			apply(other.logScore,other.term,other.left,other.right)
		}
		def nilify:Unit = { logScore = Double.NaN; term = null }
		def isNil:Boolean = (term == null)

		// -- ParseTree Properties --
		override def head:Head.Value = {
			assert(term != null,"taking head of null rule"); 
			term.head
		}
		override def children:Array[ParseTree] = {
			assert(term != null,"taking children of null rule")
			if(left == null && right == null) { //leaf
				return Array[ParseTree]()
			} else if(term.arity == 1) {
				assert(right == null, "closure with 2 children")
				Array[ParseTree](left)
			} else if(term.arity == 2) {	
				Array[ParseTree](left,right)
			} else {
				throw new IllegalStateException("Bad cky term: " + term)
			}
		}
		private def evaluateHelper(sent:Sentence,i:Int
				):(Int,(Head.Value,Any) ) = {
			assert(term != null, "evaluating null rule")
			if(term.arity == 1) {
				//(case: unary rule)
				assert(right == null, "binary rule on closure ckyI")
				var (childI,(childType,childValue)) = {
					if(isLeaf){ //<--Base Case (leaf node)
						if(sent.words(i) == G.NUM){
							//(case: number)
							(i+1,(Head.Number,sent.nums(i)))
						} else {
							//(case: word)
							(i+1,(Head.Word,sent.words(i)))
						}
					} else {
						left.evaluateHelper(sent,i)
					}
				}
				term.rules.reverse.foreach{ r =>
					assert(r.arity == 1, "closure with binary rule")
					childValue = r(childValue)
				}
				assert(childValue != null, "null value returned")
				(childI,(head,childValue))
			}else if(term.arity == 2){
				//(case: binary rule)
				assert(term.rids.length == 1, "Multi-rule non-closure")
				val r = term.rule
				assert(r.arity == 2, "non-closure unary")
				val (leftI,(leftType,leftValue)) = left.evaluateHelper(sent,i)
				val (rightI,(rightType,rightValue)) = right.evaluateHelper(sent,leftI)
				val childValue = r(leftValue,rightValue)
				assert(childValue != null, "null value returned")
				(rightI,(r.head,childValue))
			}else{
				throw new IllegalStateException("Invalid cky term")
			}
		}
		private var evalCache:(Head.Value,Temporal,Double) = null
		override def evaluate(sent:Sentence):(Head.Value,Temporal,Double) = {
			if(evalCache == null){
				val (length,(tag,value)) = evaluateHelper(sent,0)
				assert(length == sent.length, 
					"missed words in evaluation: " + length + " " + sent.length)
				assert(this.logScore <= 0.0, 
					">1.0 probability: logScore=" + this.logScore)
				val temporalVal:Temporal = value match {
					case (t:Temporal) => t
					case (fn:(Range=>Range)) => new PartialTime(fn)
				}
				evalCache = (tag,temporalVal,this.logScore)
			}
			evalCache
		}
		private def traverseHelper(i:Int,
				ruleFn:Int=>Any,lexFn:(Int,Int)=>Any,up:()=>Any):Int = {
			assert(term != null, "evaluating null rule")
			var stackDepth:Int = 0
			val pos = if(term.arity == 1) {
				//(case: unary rule)
				assert(right == null, "binary rule on closure ckyI")
				if(isLeaf) {
					assert(term.rids.length==1, "closure used as lex tag")
					lexFn(term.rid,i)
					i + 1 //return
				} else {
					term.rids.foreach{ (rid:Int) => stackDepth+=1; ruleFn(rid) }
					left.traverseHelper(i,ruleFn,lexFn,up) //return
				}
			}else if(term.arity == 2){
				term.rids.foreach{ (rid:Int) => stackDepth+=1; ruleFn(rid) }
				val leftI = left.traverseHelper(i,ruleFn,lexFn,up)
				right.traverseHelper(leftI,ruleFn,lexFn,up) //return
			}else{
				throw new IllegalStateException("Invalid cky term")
			}
			//(pop stack and return)
			(0 until stackDepth).foreach{ (rid:Int) => up() }
			pos
		}
		override def traverse(ruleFn:Int=>Any,lexFn:(Int,Int)=>Any):Unit = {
			traverseHelper(0,ruleFn,lexFn,()=>{})
		}
		override def asParseString(sent:Sentence):String = {
			val b = new StringBuilder
			//(clean string)
			def clean(str:String) = {
				"\"" + str.replaceAll("\"","\\\\\"").replaceAll("'","\\\\'") + "\""
			}
			//(traverse)
			traverseHelper(0,
				(rid:Int) => {
					b.append("(").append(clean(Grammar.RULES_STR(rid))).append(" ")
				},
				(rid:Int,w:Int) => {
					b.append("( ").append(clean(Grammar.RULES_STR(rid))).append(" ").
						append(clean(sent(w))).append(" ) ")
				},
				() => {
					b.append(") ")
				})
			b.toString
		}
		override def maxDepth:Int = {
			var maxDepth = 0
			var depth = 0
			traverseHelper(0,
				(rid:Int) => {depth += 1; maxDepth = math.max(depth,maxDepth) },
				(rid:Int,w:Int) => { },
				() => { depth -= 1 })
			maxDepth+1 // +1 for lex terms
		}
		def deepclone:ChartElem = {
			val leftClone = if(left == null) null else left.deepclone
			val rightClone = if(right == null) null else right.deepclone
			new ChartElem(logScore,term,leftClone,rightClone)
		}
		// -- Object Properties --
		override def clone:ChartElem = {
			new ChartElem(logScore,term,left,right)
		}
		override def equals(a:Any) = {
			a match {
				case (elem:ChartElem) => {
					elem.term == term && elem.left == left && elem.right == right
				}
				case (_:Any) => false
			}
		}
		override def hashCode:Int = {
			term.hashCode ^ left.hashCode
		}
		override def toString:String = {
			"[" + G.df.format(logScore) + "] " + 
				term + " -> (" + U.join(children.map{ _.head },", ") + ")"
		}
	}

	//-----
	// K-Best List
	//-----
	class BestList(values:Array[ChartElem],var capacity:Int) {
		
		// -- Lazy Eval --
		type LazyStruct = (CkyRule,BestList,BestList,(ChartElem,ChartElem)=>Double)
		private var deferred:List[LazyStruct] = null
		private var lazyNextFn:Unit=>Boolean = null
		def markLazy = { 
			assert(deferred == null, "marking as lazy twice")
			deferred = List[LazyStruct]() 
		}
		def markEvaluated = { deferred = null }
		def isLazy:Boolean = (deferred != null)
		def ensureEvaluated = { 
			if(isLazy){
				while(lazyNext){ }
				markEvaluated 
			}
			if(O.paranoid){
				val (ok,str) = check(false); assert(ok,"ensureEvaluated: " +str)
			}
		}

		// -- Structure --
		var length = 0

		def apply(i:Int) = {
			if(isLazy) {
				while(i >= length){
					if(!lazyNext){ throw new ArrayIndexOutOfBoundsException(""+i) }
				}
			} else {
				if(i > length){ throw new ArrayIndexOutOfBoundsException(""+i) }
			}
			values(i)
		}
		def has(i:Int):Boolean = {
			if(isLazy){
				while(i >= length){
					if(!lazyNext){ return false }
				}
				return true
			} else {
				return i < length
			}
		}
		def reset(newCapacity:Int):Unit = {
			length = 0
			capacity = newCapacity
			markEvaluated
		}
		def foreach(fn:ChartElem=>Any):Unit = {
			ensureEvaluated
			for(i <- 0 until length){ fn(values(i)) }
		}
		def map[A : Manifest](fn:ChartElem=>A):Array[A] = {
			ensureEvaluated
			val rtn = new Array[A](length)
			for(i <- 0 until length){
				rtn(i) = fn(values(i))
			}
			rtn
		}
		def zipWithIndex = {
			ensureEvaluated
			values.slice(0,length).zipWithIndex
		}
		def toArray:Array[ChartElem] = {
			ensureEvaluated
			values.slice(0,length)
		}
		override def clone:BestList = {
			ensureEvaluated
			var rtn = new BestList(values.clone,capacity)
			rtn.length = this.length
			rtn
		}
		def deepclone:BestList = {
			ensureEvaluated
			var rtn = new BestList(values.map{ _.clone },capacity)
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
			//(acceptable score)
			for(i <- 0 until this.length){
				if(values(i).logScore > 0 || values(i).logScore.isNaN ){ 
					return (false,"bad score for element " + i + " " + values(i).logScore)
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
					if(values(i).equals(values(j))){ 
						return (false,"not unique: " + values(i) + " versus " + values(j)) 
					}
				}
			}
			//(ok)
			return (true,"")
		}

		//<Algorithm 0>
		private def mult0(term:CkyRule, left:BestList, right:BestList,
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
			val sorted = combined.sortBy( - _._1 )
			assert(sorted.length > 0, "empty combined vector")
			if(O.paranoid){
				//(check)
				var highest:Double = Double.PositiveInfinity
				sorted.foreach{ case (score:Double,left,right) => 
					assert(!score.isNaN, "NaN score found")
					assert(score <= highest, 
						"mult0 output not sorted: " + score + " > " + highest)
					highest = score
				}
			}
			sorted
		}
		private def merge0(term:CkyRule, 
				input:Array[(Double,ChartElem,ChartElem)]):Unit = {
			assert(term != null, "Merging bad rule")
			assert(capacity > 0 && (this.length > 0 || input.length > 0),
				"bad precondition to merge")
			var defendP = 0
			var candP = 0
			var index:Int = 0
			val defender = this.deepclone
			//--Merge
			while(index < capacity && 
					(defendP < this.length ||
					candP < input.length) ){
				val takeNew = 
					if(defendP < defender.length && candP < input.length){
						//(case: either element valid)
						if(defender(defendP).logScore >= input(candP)._1){
							false
						} else {
							true
						}
					} else if(defendP < defender.length) { false //(case: only defender)
					} else if(candP < input.length) { true //(case: only candidate)
					} else { throw new IllegalStateException() }
				if(takeNew){
					//(case: take candidate)
					val (score,left,right) = input(candP)
					assert(!score.isNaN, "setting to NaN score")
					if(right == null) {
						assert(left != null, "setting to null rule")
						values(index)(score,term,left)
					} else {
						assert(left != null, "setting to null rules")
						values(index)(score,term,left,right)
					}
					index += 1; candP += 1;
				} else {
					//(case: keep defender)
					assert(!defender(defendP).logScore.isNaN, "setting to NaN score")
					values(index)(defender(defendP))
					index += 1; defendP += 1;
				}
			}
			//--Cleanup
			//(set length)
			length = index
			assert(length != 0, "Merge returned length 0")
		}
		private def algorithm0(term:CkyRule, left:BestList, right:BestList,
				score:(ChartElem,ChartElem)=>Double):Unit = {
			assert(left.length > 0, "precondition for algorithm0")
			merge0(term,mult0(term, left, right, score))
		}
		
		//<Algorithm 1>
		private def mult1(term:CkyRule, left:BestList, right:BestList,
				score:(ChartElem,ChartElem)=>Double
				):Array[(Double,ChartElem,ChartElem)] = {
			val combined:Array[(Double,ChartElem,ChartElem)] = if(term.arity == 1) {
				//--Unary Rule
				left.map{ elem => 
					(elem.logScore+score(elem,null), elem, null)
				}
			} else if(term.arity == 2) {
				//--Binary Rule
				//(setup queue)
				val pq = new PriorityQueue[(Double,Int,Int)]
				val seen = new Array[Boolean](left.length*right.length)
				def enqueue(lI:Int,rI:Int) = {
					if(	lI < left.length && 
							rI < right.length && 
							!seen(lI*right.length+rI)){
						val s 
							= left(lI).logScore+right(rI).logScore+score(left(lI),right(rI))
						pq.enqueue( (s,lI,rI) )
						seen(lI*right.length+rI) = true
					}
				}
				enqueue(0,0)
				var out = List[(Double,ChartElem,ChartElem)]()
				//(uniform cost search)
				assert(right != null, "no right child for binary rule")
				assert(left.capacity == right.capacity, "k differs between children")
				while(out.length < left.capacity && !pq.isEmpty) {
					//(dequeue)
					val (s,lI,rI) = pq.dequeue
					out = (s,left(lI),right(rI)) :: out
					//(add neighbors)
					enqueue(lI+1,rI)
					enqueue(lI,rI+1)
				}
				//(pass value up)
				assert(!pq.isEmpty || left.length*right.length <= left.capacity,
					"priority queue is prematurely empty: " + out.length)
				out.reverse.toArray
			} else {
				throw new IllegalStateException("Arity > 2 rule")
			}
			//--Sanity Checks
			assert(combined.length > 0, "empty combined vector")
			if(O.paranoid){
				//(check sorted)
				var highest:Double = Double.PositiveInfinity
				combined.foreach{ case (score:Double,left,right) => 
					assert(!score.isNaN, "NaN score found")
					assert(score <= highest, 
						"mult0 output not sorted: " + score + " > " + highest)
					highest = score
				}
				//(matches algorithm 0)
				assert(
					mult0(term, left, right, score).
						zip(combined).forall{ case ((s0,l0,r0),(s1,l1,r1)) => s0 == s1 },
					"mult0 should match up with mult1")
				//(unique elements)
				for(i <- 0 until combined.length){
					for(j <- (i+1) until combined.length){
						val (sA,lA,rA) = combined(i)
						val (sB,lB,rB) = combined(j)
						assert(!(sA == sB && lA == lB && rA == rB), 
							"duplicate in mult1: " + i + ", " + j)
					}
				}
			}
			combined
		}
		private def algorithm1(term:CkyRule, left:BestList, right:BestList,
				score:(ChartElem,ChartElem)=>Double):Unit = {
			assert(left.length > 0, "precondition for algorithm1")
			merge0(term,mult1(term, left, right, score))
		}
		
		//<Algorithm 2>
		private def lazyNext:Boolean = {
			assert(isLazy, "Lazy next called on non-lazy structure")
			if(lazyNextFn == null){ lazyNextFn = mkLazyNext }
			lazyNextFn()
		}
		private def mkLazyNext:Unit=>Boolean = {
			assert(isLazy, "mkLazy called on non-lazy structure")
			//--State
			//(bookeeping)
			var lazyArray:Array[LazyStruct] = deferred.toArray
			//(check)
			if(O.paranoid){
				for(i <- 0 until lazyArray.length){
					val (rl1,l1,r1,fn1) = lazyArray(i)
					for(j <- (i+1) until lazyArray.length){
						val (rl2,l2,r2,fn1) = lazyArray(j)
						assert(rl1 != rl2 || l1 != l2 || r1 != r2,"duplicates in lazyArray")
					}
				}
			}
			assert(length == 0, "mklazy called with existing length")
			//(priority queue)
			case class DataSource(score:Double,source:Int,leftI:Int,rightI:Int
					) extends Ordered[DataSource] {
				def compare(that:DataSource):Int = {
					if(this.score < that.score) -1
					else if(this.score > that.score) 1
					else 0
				}
			}
			var pq = new PriorityQueue[DataSource]
			var seen = new HashSet[Int]
			var seenUnary = new HashSet[Int]
//			var seen = new Array[Boolean](lazyArray.length*capacity*capacity)
//			var seenUnary = new Array[Boolean](lazyArray.length*capacity)
			//(enqueue method)
			def enqueue(source:Int,lI:Int,rI:Int) = {
				val (rule,left,right,score) = lazyArray(source)
				if(	left.has(lI) &&
						( ( right == null && 
						    !seenUnary( source*capacity + lI ) ) ||
							( ( right != null && right.has(rI) &&
						      !seen(	source * capacity * capacity + 
						              lI * capacity +
									  	    rI			) ) ) )
							){
					val s:Double = if(right == null) {
							seenUnary( source*capacity + lI ) = true
							left(lI).logScore+score(left(lI),null)
						} else {
							seen(	source * capacity * capacity + 
								        lI * capacity +
												rI			) = true
							left(lI).logScore+right(rI).logScore+score(left(lI),right(rI))
						}
					pq.enqueue( DataSource(s,source,lI,rI) ) //<--actual enqueue
				}
			}
			//(initialize queue)
			for(i <- 0 until lazyArray.length) { enqueue(i,0,0) }
			//--Function
			(Unit) => {
				if(length >= capacity) {
					//(too long)
					false
				} else if(pq.isEmpty) {
					//(no more terms to evaluate)
					if(O.paranoid){
						val potentialSize = lazyArray.foldLeft(0){ 
							case (sizeSoFar, (term,left,right,score)) => 
								left.ensureEvaluated
								var size = left.length
								if(right != null){
									right.ensureEvaluated
									size *= right.length
								}
								sizeSoFar + size }
						assert(potentialSize == length, "pq did not exhaust options: " + 
							potentialSize + ", used " + length + " rules " + lazyArray.length)
					}
					false
				} else {
					//(dequeue)
					val datum = pq.dequeue
					//(process datum)
					val (rule,left,right,score) = lazyArray(datum.source)
					val lI = datum.leftI
					val rI = datum.rightI
					if(rule.arity == 1){
						values(length)(datum.score,rule,left(lI))
					} else {
						values(length)(datum.score,rule,left(lI),right(rI))
					}
					length += 1
					//(add neighbors) //note: checks are done in enqueue
					assert(datum.source < lazyArray.length, "source out of bounds")
					enqueue(datum.source,lI+1,rI)
					enqueue(datum.source,lI,rI+1)
					//(return)
					true
				}
			}
		}

		private def algorithm2(term:CkyRule, left:BestList, right:BestList,
				score:(ChartElem,ChartElem)=>Double):Unit = {
			algorithm3(term,left,right,score)
		}

		//<Algorithm 3>
		private def algorithm3(term:CkyRule, left:BestList, right:BestList,
				score:(ChartElem,ChartElem)=>Double):Unit = {
			if(!isLazy){ this.markLazy }
			deferred = (term,left,right,score) :: deferred
		}

		//<Top Level>
		def combine(term:CkyRule, left:BestList, right:BestList,
				score:(ChartElem,ChartElem)=>Double):Unit = {
			assert(term.arity == 2 || right == null, "unary rule has 2 children")
			assert(term.arity == 1 || right != null, "binary rule has 1 child")
			O.kbestCKYAlgorithm match{
				case 0 => if(left.length > 0 && (right == null || right.length > 0)) {
					if(O.paranoid){val (ok,str) = check(false); assert(ok,"pre: " + str)}
					this.algorithm0(term, left, right, score)
					if(O.paranoid){val (ok,str) = check(); assert(ok,"post: " + str)}
				}
				case 1 => if(left.length > 0 && (right == null || right.length > 0)) {
					if(O.paranoid){val (ok,str) = check(false); assert(ok,"pre: " + str)}
					this.algorithm1(term, left, right, score)
					if(O.paranoid){val (ok,str) = check(); assert(ok,"post: " + str)}
				}
				case 2 => this.algorithm2(term, left, right, score)
				case 3 => this.algorithm3(term, left, right, score)
				case _ => throw fail("bad algorithm: " + O.kbestCKYAlgorithm)
			}
		}
		def combine(term:CkyRule, left:BestList,
				score:(ChartElem,ChartElem)=>Double):Unit = {
			assert(term.arity == 1, "must be arity 1 rule")
			combine(term, left, null, score)
		}

		// -- Standard Methods --
		def add(score:Double,term:CkyRule,left:ChartElem,right:ChartElem) = {
			assert(term.arity == 2, "must be arity 2 rule")
			values(length)(score,term,left,right)
			length += 1
		}
		def add(score:Double,term:CkyRule,left:ChartElem) = {
			assert(term.arity == 1, "must be arity 1 rule")
			values(length)(score,term,left)
			length += 1
		}
		def suggest(score:Double,term:CkyRule,left:ChartElem,right:ChartElem) = {
			if(length < capacity){ add(score,term,left,right) }
		}
		def suggest(score:Double,term:CkyRule,left:ChartElem) = {
			if(length < capacity){ add(score,term,left) }
		}
		def suggest(score:Double,term:CkyRule) = {
			if(length < capacity){ add(score,term,null) }
		}
	}


	//-----
	// Chart
	//-----
	type RuleList = Array[BestList]
	type RulePairList = Array[RuleList]
	type Chart = Array[Array[RulePairList]]
	
	val makeChart:(Int,Int)=>Chart = { //start,length,split
		var largestChart = new Chart(0)
		var largestBeam = 0
		(inputLength:Int,inputBeam:Int) => {
			//--Make Chart
			val chart = if(inputLength > largestChart.length || 
					inputBeam > largestBeam){ 
				val len = math.max(inputLength, largestChart.length)
				val beam = math.max(inputBeam, largestBeam)
				//(create)
				largestChart = (0 until len).map{ (start:Int) =>            //begin
					assert(len-start > 0, "bad length end on start "+start+" len "+len)
					(0 until (len-start)).map{ (length:Int) =>                //length
						assert(Head.values.size > 0, "bad rules end")
						(0 to 1).map{ (arity:Int) =>                            //arity
							(0 until Head.values.size).map{ (rid:Int) =>          //rules
								assert(beam > 0, "bad kbest end")
								new BestList((0 until beam).map{ (kbestItem:Int) => //kbest
									new ChartElem
								}.toArray, beam) //convert to arrays
							}.toArray
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
			for(start <- 0 until inputLength){
				for(len <- 0 until chart(start).length){
					for(head <- 0 until chart(start)(len).length){
						chart(start)(len)(UNARY)(head).reset(inputBeam)
						chart(start)(len)(BINARY)(head).reset(inputBeam)
						assert( chart(start)(len)(UNARY)(head) !=
							chart(start)(len)(BINARY)(head), "corrupted chart")
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
	def gram(chart:Chart,begin:Int,end:Int,head:Int,t:Int):BestList = {
		if(end == begin+1){ return lex(chart,begin,head,t) }
		//(asserts)
		assert(end > begin+1, "Chart access error: bad end: " + begin + ", " + end)
		assert(begin >= 0, "Chart access error: negative values: " + begin)
		assert(head >= 0, "Chart access error: bad head: " + head)
		assert(head < Head.values.size, "Chart access error: bad head: " + head)
		assert(t == 0 || t == 1, "must be one of UNARY/BINARY")
		//(access)
		chart(begin)(end-begin-1)(t)(head)
	}
	def lex(chart:Chart,elem:Int,head:Int,t:Int=BINARY):BestList = {
		//(asserts)
		assert(elem >= 0, "Chart access error: negative value: " + elem)
		assert(head >= 0, "Chart access error: bad head: " + head)
		assert(head < Head.values.size, "Chart access error: bad head: " + head)
		chart(elem)(0)(t)(head)
	}
	
	//-----
	// Learning
	//-----

	
	
	def lexLogProb(w:Int,pos:Int,rule:CkyRule):Double = {
		assert(rule.arity == 1, "Lex with binary rule")
		assert(w < G.W || w == G.UNK, "Word is out of range: " + w)
		val raw:Double = wordScores(rule.rid)(w)
		val W:Double = wordScores(rule.rid).length.asInstanceOf[Double]
		val totalCount:Double = wordTotalCounts(rule.rid)
		val prob:Double = O.smoothing match {
			case O.SmoothingType.none => raw
			case O.SmoothingType.addOne => (raw*totalCount+1.0)/(totalCount+W)
			case _ => 
				throw new IllegalStateException("Unknown smoothing: " + O.smoothing)
		}
		U.safeLn(prob)
	}
	def ruleLogProb(rule:CkyRule):Double = {
		rule.rids.foldLeft(0.0){ (logScore:Double,rid:Int) => 
			assert(!RULES(rid).isLex,
				"Rule probability calculation accessed a lex rule")
			logScore + U.safeLn(ruleScores(rid)) }
	}

	def klex(sent:Sentence,y:(CkyRule,Int,Double)=>Boolean):Array[Int] = {
		(0 until sent.length).map{ (elem:Int) => 
			val word:Int = sent.words(elem)
			val pos:Int = sent.pos(elem)
			val num:Int = sent.nums(elem)
			//(get candidate parses)
			val candidates = CKY_LEX
				.filter{ (term:CkyRule) =>
					(  term.child == Head.Word ||
					   (term.child == Head.Number && word == G.NUM) ) &&  //is valid rule
					term.validInput( if(word == G.NUM) num else word ) }  //is in range
				.map{ (term:CkyRule) => (term, lexLogProb(word,pos,term)) }
			//(yield)
			var ok:Boolean = true
			candidates.sortBy( - _._2).foreach{ case (term,score) => 
				assert(term.isLex, "bad term returned in klex")
				if(ok && !y(term,elem,score)){ ok = false }
			}
			candidates.length
		}.toArray
	}
}

class CKYParser extends StandardParser{
	import CKYParser._
	import Grammar._
	//-----
	// Startup
	//-----
	//(normalize)
	normalize

	//-----
	// CKY
	//-----
	def cky[T](sent:Sentence,beam:Int):Array[ParseTree] = {
		//--Create Chart
		val chart = makeChart(sent.length,beam)
		assert(chart.length >= sent.length, "Chart is too small")
		//--Lex
		//(get probability function)
		val lexLogProb:(Sentence,(CkyRule,Int,Double)=>Boolean)=>Array[Int]
			= O.lexTagMethod match {
				case O.TagMethod.PCFG => CKYParser.klex(_,_)
				case O.TagMethod.CRF  => 
					if(tagger == null) CKYParser.klex(_,_)
					else tagger.klex(_,O.crfKBest,_)
				case O.TagMethod.GOLD =>
					Const.goldTag
			}
		//(fill chart)
		val lastScore:Array[Double] = (0 until sent.length).map{ 
			(i:Int) => Double.PositiveInfinity }.toArray
		lexLogProb(sent, (term:CkyRule,elem:Int,score:Double) => {
			//(add terms)
			assert(score <= 0.0, "Lex probability of >0: " + score)
			lex(chart,elem,term.head.id).suggest(score,term)
			assert(score <= lastScore(elem),
				"KLex out of order: "+lastScore(elem)+"->"+score);
			lastScore(elem) = score
			true
		})
		//(check)
		for(elem <- 0 until sent.length) {
			if(O.paranoid){
				var count:Int = 0
				Head.values.foreach{ head:Head.Value => 
					count += lex(chart,elem,head.id).length
				}
				assert(count > 0, "Word " + elem + 
					" ("+U.w2str(sent.words(elem))+") should have lex completions")
			}
		}
		//--Grammar
		for(length <- 1 to sent.length) {                      // length
			for(begin <- 0 to sent.length-length) {              // begin
				val end:Int = begin+length
				assert(end <= sent.length, "end is out of bounds")
				//(update chart)
				CKY_BINARY.foreach{ (term:CkyRule) =>              // rules [binary]
					val ruleLProb:Double = ruleLogProb(term)
					assert(term.arity == 2, "Binary rule should be binary")
					val r = term.rule
					for(split <- (begin+1) to (end-1)){              // splits
						val leftU:BestList = gram(chart,  begin, split, r.left.id,  UNARY)
						val rightU:BestList = gram(chart, split, end,   r.right.id, UNARY)
						val leftB:BestList = gram(chart,  begin, split, r.left.id,  BINARY)
						val rightB:BestList = gram(chart, split, end,   r.right.id, BINARY)
						assert(leftU != leftB && rightU != rightB, ""+begin+" to "+end)
						val output = gram(chart,begin,end,r.head.id,BINARY)
						val score = (left:ChartElem,right:ChartElem) => { ruleLProb }
						output.combine(term,leftU,rightU,score)
						output.combine(term,leftU,rightB,score)
						output.combine(term,leftB,rightU,score)
						output.combine(term,leftB,rightB,score)
					}
				}
				CKY_UNARY.foreach{ (term:CkyRule) =>               // rules [unary]
					val ruleLProb = ruleLogProb(term)
					assert(term.arity == 1, "Unary rule should be unary")
					val child:BestList = gram(chart,begin,end,term.child.id,BINARY)
					gram(chart,begin,end,term.head.id,UNARY).combine(term,child,
						(left:ChartElem,right:ChartElem) => { ruleLProb })
				}
				//(post-update tasks)
				if(O.kbestCKYAlgorithm < 3) {
					Head.values.foreach { head => 
						gram(chart,begin,end,head.id,BINARY).ensureEvaluated
						gram(chart,begin,end,head.id,UNARY).ensureEvaluated
					}
				}
			}
		}
		//--Return
		Array.concat(
			gram(chart,0,sent.length,Head.ROOT.id,UNARY).toArray,
			gram(chart,0,sent.length,Head.ROOT.id,BINARY).toArray
			).map{ x => x.deepclone }
	}

	//-----
	// Parse Method
	//-----

	override def beginIteration(iter:Int,feedback:Boolean,data:DataStore):Unit = {
		start_track("Begin Iteration")
		//(debug rules)
		ruleScores.zipWithIndex.foreach{ case (score:Double,rid:Int) =>
			debugG("rule["+RULES_STR(rid)+"] score " + score)
		}
		CKY_UNARY.foreach{ (r:CkyRule) => 
			debugG("unary [" + G.df.format(ruleLogProb(r))+"]  "+r.rids.length+" "+r)
		}
		CKY_LEX.foreach{ (r:CkyRule) => 
			debugG("lex "+RULES_STR(r.rid))
		}
		//(clear last decoded)
		log("clearing last parses")
		if(feedback){
			guesses = List[GuessInfo]()
			corrects = List[GuessInfo]()
		}
		end_track
	}

	override def endIteration(iter:Int,feedback:Boolean,data:DataStore):Unit = {
		if(feedback){
			log("updating counts")
			//--Copy Weights
			//(rules)
			(0 until ruleScores.length).foreach{ (rid:Int) => ruleScores(rid) = 0.0 }
			rulesCounted.zipWithIndex.foreach{ case (count,rid) =>
				ruleScores(rid) += count
			}
			//(lex)
			wordsCounted.zipWithIndex.foreach{ case (counter,rid) =>
				(0 until wordScores(rid).length).foreach{ (w:Int) => 
					wordScores(rid)(w) = 0.0 
				}
				counter.keySet.foreach{ (w:Int) =>
					wordScores(rid)(w) = counter.getCount(w)
				}
				wordTotalCounts(rid) = counter.totalCount
			}
			//(clear counts)
			(0 until rulesCounted.length).foreach{ (rid:Int) => rulesCounted(rid)=0.0}
			(0 until wordsCounted.length).foreach{ (rid:Int) => 
				wordsCounted(rid) = new ClassicCounter[Int]
			}
			//(normalize)
			log("normalizing (M Step)")
			normalize
		}
		//--Debug
		start_track("Iteration Summary")
		reportInternal(false)
		end_track
	}


	private def reportInternal(writeParses:Boolean):Unit = {
		//--Debug Print
		//(best lex)
		start_track("lex scores (top)")
		Head.values.foreach{ (head:Head.Value) =>
			val bestLex:List[(Int,Int,Double)] = CKY_LEX
				.filter{ (r:CkyRule) => r.head==head }
				.foldLeft(List[(Int,Int,Double)]()){ 
						(soFar:List[(Int,Int,Double)], r:CkyRule) =>
					soFar ::: 
						wordScores(r.rid).zipWithIndex.map{ case (score:Double,w:Int) =>
							(r.rid,w,score) }.toList
				}.filter{ _._3 > 0.0 }.sortBy( - _._3 ).slice(0,5)
			bestLex.foreach{ case (rid,w,score) =>
				logG("[" + G.df.format(score) + "] " + 
					U.w2str(w) + " from " + RULES_STR(rid) )
			}
		}
		end_track
		//(best rules)
		start_track("rule scores (top)")
		ruleScores.zipWithIndex.sortBy(-_._1).slice(0,10).foreach{case (score,rid)=>
			logG("[" + G.df.format(score) + "] " + RULES(rid))
		}
		end_track
		//(write guesses)
		if(writeParses){
			start_track("Creating Presentation")
			val b = new StringBuilder
			b.append(Const.START_PRESENTATION("Results"))
			guesses.sortBy( _.sid ).foreach{ (g:GuessInfo) =>
				val ref = if(g.feedback == null) null else g.feedback.ref
				if(g.isFailedParse){
					//(case: no parses for sentence)
					b.append(Const.AUTO_MISS(g.sid,g.sent,ref))
				} else {
					if(Parse(Head.ROOT,g.feedback.ref) != null){
						assert(g.feedback != null, "No feedback stored with " + g)
						assert(g.feedback.grounding != null, "No grounding stored with "+g)
						//(case: parsed sentence)
						b.append(Const.SLIDE(
								id=g.sid, correct=g.correct, tree=g.parse.asParseString(g.sent),
								guess=g.parseVal.ground(g.feedback.grounding).toString,
								gold=Parse(Head.ROOT,g.feedback.ref)
									.ground(g.feedback.grounding).toString,
								ground=g.feedback.grounding.toString
							))
					} else {
						//(case: sentence is UNK)
						b.append(Const.AUTO_MISS(g.sid,g.sent,ref))
					}
				}
			}
			b.append(Const.END_PRESENTATION)
			val writer = new java.io.FileWriter(Execution.touch("parses.rb"))
			writer.write(b.toString)
			writer.close
			end_track
		}
	}

	override def report = reportInternal(true)

	private def isEquivalentOutput(
			guess:Array[(Head.Value,Temporal,Double)],
			gold:Array[(Head.Value,Temporal,Double)]   ):(Boolean,String) = {
		if(guess.length != gold.length){ return (false,"different lengths") }
		//--Set Equality
		def setEquality(begin:Int,end:Int):(Boolean,String) = {
			for(i <- begin until end){
				if( !(begin until end).exists{ (j:Int) => 
							val (guessH,guessV,guessS) = guess(i)
							val (goldH,goldV,goldS) = gold(j)
							Parse(guessH,guessV).equals( Parse(goldH,goldV) )
						} ) {
					return (false, "no reference match for guess " + i)
				}
			}
			return (true,"ok")
		}
		//--Loop
		var score:Double = Double.NaN
		var begin:Int = 0
		var end:Int = 0
		for(i <- 0 until guess.length) {
			val (headGuess,parseGuess,scoreGuess) = guess(i)
			val (headGold,parseGold,scoreGold) = gold(i)
			if(scoreGuess != scoreGold){ return (false,"mismatched scores: " + i) }
			if(scoreGuess != score) {
				//(new score)
				val (ok,str) = setEquality(begin,end)
				if(!ok){ return (ok,str) }
				begin = i
				end = i+1
				score = scoreGuess
			} else {
				//(same score)
				end += 1
			}
		}
		//--Last Check / Return
		if(guess.length < O.beam){ //things can dangle off the end
			setEquality(begin,end)
		} else {
			(true,"ok")
		}
	}


	def ensureValidProbabilities:(Boolean,String) = {
		def isOne(d:Double):Boolean = d > 0.99999 && d < 1.00001
		import scala.math.exp
		//--Raw Scores
		//(rules)
		Head.values.foreach{ (head:Head.Value) =>
			val unaryScore:Double = CKY_UNARY
				.filter{ (r:CkyRule) => r.head==head && r.rids.length == 1}
				.foldLeft(0.0){ (soFar:Double,r:CkyRule) => soFar + ruleScores(r.rid) }
			val binaryScore:Double = CKY_BINARY
				.filter{ (r:CkyRule) => r.head==head && r.rids.length == 1}
				.foldLeft(0.0){ (soFar:Double,r:CkyRule) => soFar + ruleScores(r.rid) }
			val totalNum:Int = 
				CKY_UNARY.filter{(r:CkyRule)=>r.head==head && r.rids.length==1}.length+
				CKY_BINARY.filter{(r:CkyRule)=>r.head==head && r.rids.length==1}.length
			if(totalNum > 0 && !isOne(unaryScore+binaryScore)){
				return (false,
					"rule (struct) scores do not add to one for head " + head + 
					" [" + (unaryScore+binaryScore)+"]")
			}
		}
		//(lex)
		Head.values.foreach{ (head:Head.Value) =>
			val lexScore:Double = CKY_LEX.filter{ (r:CkyRule) => r.head==head }
				.foldLeft(0.0){ (soFar:Double, r:CkyRule) =>
					soFar + wordScores(r.rid).foldLeft(0.0){ (x:Double,s:Double) => x+s }
				}
			val num:Int = CKY_LEX.filter{ (r:CkyRule) => r.head==head }.length
			if(num>0 && !isOne(lexScore)){
				return (false,"lex (struct) scores for nonterminal " +
					head + " does not sum to 1 ["+lexScore+"]")
			}
		}
		//--Functions
		//(rules)
		Head.values.foreach{ (head:Head.Value) =>
			val unaryScore:Double = CKY_UNARY
				.filter{ (r:CkyRule) => r.head==head && r.rids.length == 1}
				.foldLeft(0.0){ (soFar:Double,r:CkyRule) => soFar+exp(ruleLogProb(r)) }
			val binaryScore:Double = CKY_BINARY
				.filter{ (r:CkyRule) => r.head==head && r.rids.length == 1}
				.foldLeft(0.0){ (soFar:Double,r:CkyRule) => soFar+exp(ruleLogProb(r)) }
			val totalNum:Int = 
				CKY_UNARY.filter{(r:CkyRule)=>r.head==head && r.rids.length==1}.length+
				CKY_BINARY.filter{(r:CkyRule)=>r.head==head && r.rids.length==1}.length
			if(totalNum > 0 && !isOne(unaryScore+binaryScore)){
				return (false,"rule (fn) scores do not add to one for head " + head +
					" [" + (unaryScore+binaryScore)+"]")
			}
		}
		//(lex)
		Head.values.foreach{ (head:Head.Value) =>
			val lexScore:Double = CKY_LEX.filter{ (r:CkyRule) => r.head==head }
				.foldLeft(0.0){ (soFar:Double, r:CkyRule) =>
					soFar + (0 to G.W).foldLeft(0.0){ (soFarWord:Double,w:Int) => 
						soFarWord+exp(lexLogProb(w,-1,r))
					}
				}
			val totalNum:Int = CKY_LEX.filter{ (r:CkyRule) => r.head==head }.length
			if(totalNum > 0 && !isOne(lexScore)){
				return (false,"lex (fn) scores for nonterminal " +
					head + " does not sum to 1 ["+lexScore+"]")
			}
		}
		(true,"ok")
	}
	
	def normalize:Unit = {
		//--Rules
		val seen = new Array[Boolean](RULES.length)
		Head.values.foreach{ (head:Head.Value) => 
			//(count)
			val unaryCount:Double = CKY_UNARY
				.filter{ (r:CkyRule) => r.head==head && r.rids.length == 1}
				.foldLeft(0.0){ (soFar:Double,r:CkyRule) => soFar + ruleScores(r.rid) }
			val binaryCount:Double = CKY_BINARY
				.filter{ (r:CkyRule) => r.head==head && r.rids.length == 1}
				.foldLeft(0.0){ (soFar:Double,r:CkyRule) => soFar + ruleScores(r.rid) }
			val totalCount:Double = unaryCount+binaryCount
			val totalNum:Int = 
				CKY_UNARY.filter{(r:CkyRule)=>r.head==head && r.rids.length==1}.length+
				CKY_BINARY.filter{(r:CkyRule)=>r.head==head && r.rids.length==1}.length
			//(set)
			CKY_UNARY.filter{ (r:CkyRule) => r.head==head && r.rids.length == 1}
				.foreach{ (r:CkyRule) => 
					assert(!seen(r.rid), "updating a rule multiple times")
					seen(r.rid) = true
					if(totalCount == 0.0){
						//(case: never seen)
						ruleScores(r.rid) = 1.0 / totalNum.asInstanceOf[Double]
					} else {
						//(case: normal)
						ruleScores(r.rid) /= totalCount
					}
				}
			CKY_BINARY.filter{ (r:CkyRule) => r.head==head}
				.foreach{ (r:CkyRule) => 
					assert(r.rids.length == 1, "binary ckyrule with multiple rules")
					assert(!seen(r.rid), "updating a rule multiple times")
					seen(r.rid) = true
					if(totalCount == 0.0){
						//(case: never seen)
						ruleScores(r.rid) = 1.0 / totalNum.asInstanceOf[Double]
					} else {
						//(case: normal)
						ruleScores(r.rid) /= totalCount
					}
				}
		}
		assert(seen.zipWithIndex.forall{ case (seen:Boolean,rid:Int) => 
				seen || RULES(rid).isLex
			}, "Some rules were not seen")
		//--Lex
		Head.values.foreach{ (head:Head.Value) =>
			//(count)
			val totalCount:Double = CKY_LEX.filter{ (r:CkyRule) => r.head==head }
				.foldLeft(0.0){ (soFar:Double, r:CkyRule) =>
					soFar + wordScores(r.rid).foldLeft(0.0){ (x:Double,s:Double) => x+s }
				}
			val num:Int = CKY_LEX.filter{ (r:CkyRule) => r.head==head }.length
			//(set)
			CKY_LEX.filter{ (r:CkyRule) => r.head==head }.foreach{ (r:CkyRule) =>
				(0 until wordScores(r.rid).length).foreach{ (w:Int) =>
					if(num > 0 && totalCount > 0.0){
						//(case: normal)
						wordScores(r.rid)(w) /= totalCount
					} else if(num > 0) {
						//(case: never seen)
						wordScores(r.rid)(w) = 1.0 /
								( wordScores(r.rid).length.asInstanceOf[Double] *
								  num.asInstanceOf[Double] )
					} else {
						throw new IllegalStateException("Should never reach here")
					}
				}
			}
		}
		//--CRF Retrain
		if(O.crfTag && corrects.length > 0){
			tagger = CRFTagger(corrects.map{ (guess:GuessInfo) =>
					val sent:Sentence = guess.sent
					val rules:Array[Int] = guess.parse.lexRules
					assert(guess.sent.length == rules.length, "length mismatch")
					( guess.sent, rules )
				}.toArray)
		} else {
			tagger = null
		}
		//--Debug
		if(O.paranoid){
			val (ok,msg) = ensureValidProbabilities; assert(ok,msg)
		}
	}


	override def parse(i:Int, sent:Sentence, feedback:Boolean, identifier:Int
			):(Array[Parse],Feedback=>Any)={
		//--Initial Checks
		if(O.paranoid){ val (ok,msg) = ensureValidProbabilities; assert(ok,msg) }
		//--Run Parser
		//(run CKY)
		val trees:Array[ParseTree] = cky(sent,O.beam)
		//(check: single-best consistency)
		if(O.paranoid && trees.length > 0){
			val singleBest:Array[ParseTree] = cky(sent,1)
			assert(singleBest.length > 0, "single best returned no tree")
			assert(singleBest.length == 1, "single best returned > 1 tree")
			assert(trees(0).equals(singleBest(0)), "parse doesn't match single-best")
		}
		//(convert to parses)
		val scored:Array[(Head.Value,Temporal,Double)]=trees.map{ _.evaluate(sent) }
		val parses:Array[Parse] = scored.map{case (tag,parse,s) => Parse(tag,parse)}
		//(debug)
		val str = 
			"Parsed \"" + U.sent2str(sent.words) + "\" ("+parses.length+") as " + 
			U.join(
				scored.slice(0,3).map{ case (tag,parse,score) => 
					""+parse+"["+G.df.format(score)+"]"}, " or ")
		if(O.printAllParses) {
			logG(str)
		} else {
			log(str)
		}
		//(check: algorithm0 consistency)
		if(O.paranoid){
			val saveAlg = O.kbestCKYAlgorithm
			O.kbestCKYAlgorithm = 0
			val reference = cky(sent,O.beam)
			val scoredReference = reference.map{ _.evaluate(sent) }
			assert(reference.length == scored.length, 
				"Algorithm different size from algorithm 0")
			assert( scored.zip(scoredReference).forall{ 
				case ((tag,value,score),(refTag,refValue,refScore)) => 
					score == refScore
				}, "Algorithm differed in scores from algorithm 0"  )
			val (equivalent,message) = isEquivalentOutput(scoredReference, scored)
			if(!equivalent){
				println("----ALGORITHM 0----")
				println(U.join(scoredReference, "\n"))
				println("----ALGORITHM "+saveAlg+"----")
				println(U.join(scored, "\n"))
			}
			assert( equivalent, "Inconsistent with algorithm 0: " + message)
			O.kbestCKYAlgorithm = saveAlg
		}
		//--Debug (begin)
		//(presentation)
		val b = new StringBuilder //debug start
		b.append(Const.START_PRESENTATION("Correct Parses, sid " + identifier))
		//--Return / Feedback
		//(add 'no result' guess)
		guesses = GuessInfo(identifier,null,Double.NaN,sent).wrong :: guesses
		//(return)
		val rtn = (	parses, 
			(feedback:Feedback) => {
				//(best guess)
				val (head,parse,score) = scored(0)
				val guess = GuessInfo(identifier,trees(0),score,sent).feedback(feedback)
				guesses = {if(feedback.isCorrect) guess else guess.wrong} ::
					guesses.tail //note: tail to remove 'no result' guess
				//(update)
				def update(index:Int) = {
					val incr:Double = if(O.hardEM) 1.0 else math.exp(scored(index)._3)
					assert(incr >= 0.0 && incr <= 1.0, "invalid increment: " + incr)
					//(count rules)
					trees(index).traverse( 
							{(rid:Int) => rulesCounted(rid) += incr},
							{(rid:Int,i:Int) => 
								wordsCounted(rid).incrementCount(sent.words(i),incr)
							}
						)
					//(append guesses)
					val score = scored(index)._3
					val parse = trees(index)
					corrects = GuessInfo(identifier,parse,score,sent) :: corrects
					//(debug)
					b.append(Const.SLIDE(
							id=identifier, correct=true, tree=parse.asParseString(sent),
							guess=parses(index).ground(feedback.grounding).toString,
							gold=Parse(Head.ROOT,feedback.ref)
								.ground(feedback.grounding).toString,
							ground=feedback.grounding.toString
						))
				}
				//(run update)
				O.ckyCountType match {
					case O.CkyCountType.all => 
						//(update every correct parse)
						feedback.correct.foreach{ case (index,score) => update(index) }
					case O.CkyCountType.bestAll =>
						//(update every tied-for-best parse)
						feedback.tiedBest.foreach{ (index:Int) => update(index) }
					case O.CkyCountType.bestRandom =>
						//(update the 'first' tied-for-best parse)
						if(feedback.hasCorrect){update(feedback.bestIndex) }
					case O.CkyCountType.bestShallow =>
						//(update the shallowest tied-for-best parse)
						val (best,depth) =
							feedback.tiedBest.foldLeft((-1,Int.MaxValue)){
								case ((argmin,min),cand) =>
									val candMin = trees(cand).maxDepth
									if(candMin < min) (cand,candMin) else (argmin,min)
								}
						assert(!feedback.hasCorrect || best >= 0, "Bad shallow computation")
						if(best >= 0){ update(best) }
					case _ => throw fail("Unknown case")
				}
			}
		)
		//--Debug (end)
		//(end presentation)
		b.append(Const.END_PRESENTATION)
		//(write presentation)
		val writer = new java.io.FileWriter(
			Execution.touch("iteration"+i+"/datum"+identifier+".rb"))
		writer.write(b.toString)
		writer.close
		//--Return
		rtn
	}
}
