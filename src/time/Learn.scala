package time

import java.util.concurrent.locks.ReentrantLock
import java.util.Calendar

import scala.collection.JavaConversions._
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

import Lex._

import org.goobs.exec.Execution
import org.goobs.stanford.JavaNLP._
import org.goobs.stats._
import org.goobs.utils.Indexer

import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.ling.CoreAnnotations.CalendarAnnotation
import edu.stanford.nlp.ling.CoreAnnotations.DocDateAnnotation
import edu.stanford.nlp.sequences.SeqClassifierFlags
import edu.stanford.nlp.sequences.FeatureFactory
import edu.stanford.nlp.time.JodaTimeUtils
import edu.stanford.nlp.util.logging.Redwood.Util._

//------------------------------------------------------------------------------
// GRAMMAR
//------------------------------------------------------------------------------

case class Nonterminal(name:Symbol,id:Int){
	//--Extra Variables
	var isPreterminal = false
	var isNil = false
	//--Methods
	def nilWord:String = name.name.substring(4)
	//--Default Overrides
	override def toString:String = name.name
}


object Nonterminal {
	private var nextId = -1
	val valueMap = new HashMap[Symbol,Nonterminal]
	var values:Array[Nonterminal] = Array[Nonterminal]()
	def register(head:Nonterminal):Nonterminal = {
		values = (values.toList ::: List[Nonterminal](head)).toArray
		valueMap(head.name) = head
		head
	}
	def apply(name:Symbol):Nonterminal = {
		valueMap(name)
	}
	def apply(name:String):Nonterminal = {
		apply(Symbol(name))
	}
	def apply(name:Symbol, t:Symbol):Nonterminal = {
		nextId += 1
		val term = new Nonterminal(name,nextId)
		t match {
			case 'nil => 
				term.isPreterminal = true
				term.isNil = true
			case 'preterminal => 
				term.isPreterminal = true
			case 'none => //noop
			case _ => throw fail("Unknown nonterminal type: " + t)
		}
		register(term)
	}
	def fromShort(short:String):Nonterminal = fromShort(Symbol(short))
	def fromShort(short:Symbol):Nonterminal = {
		short match {
			case 'R => Nonterminal('Range)
			case 'S => Nonterminal('Sequence)
			case 'D => Nonterminal('Duration)
			case _ => throw fail("No such short form: " + short)
		}
	}
	def name(term:Nonterminal):String = {
		term.name match {
			case 'Range => "R"
			case 'Duration => "D"
			case 'Sequence => "S"
			case _ => term.name.name
		}
	}
	def lexicalize(indexer:Indexer[String]) = {
		if(O.lexNils){
			indexer.foreach{ (word:String) =>
				Nonterminal(Symbol("NIL-"+word), 'nil)
			}
		} else {
			Nonterminal('NIL, 'nil)
		}
	}

	//--Create Nonterminals
	val ranges = List("R","S")
	val durations = List("D","S")
	//(root)
	Nonterminal('ROOT, 'none)
	//(lex)
	Nonterminal('Word, 'preterminal)
	Nonterminal('Number, 'preterminal)
	//(basic types)
	Nonterminal('Range, 'none)
	Nonterminal('Duration, 'none)
	Nonterminal('Sequence, 'none)
	//(arity-2 functions)
	//((like rd2r))
	val fn2 = ranges.foldLeft(List[(Nonterminal,Symbol,Symbol)]()){
			case (soFar:List[(Nonterminal,Symbol,Symbol)],r:String) =>
		durations.foldLeft(List[(Nonterminal,Symbol,Symbol)]()){
				case (soFar:List[(Nonterminal,Symbol,Symbol)],d:String) =>
			( Nonterminal(Symbol("F_{"+r+d+"}2"+r), 'none),Symbol(r),Symbol(d)
				) :: soFar
		} ::: soFar
	} ::: 
	//((like rr2r))
	ranges.foldLeft(List[(Nonterminal,Symbol,Symbol)]()){
			case (soFar:List[(Nonterminal,Symbol,Symbol)],r1:String) =>
		ranges.foldLeft(List[(Nonterminal,Symbol,Symbol)]()){
				case (soFar:List[(Nonterminal,Symbol,Symbol)],r2:String) =>
			(Nonterminal(Symbol("F_{"+r1+r2+"}2"+r1),'none),
				Symbol(r1),Symbol(r2)) :: soFar
		} ::: soFar
	}
	//(arity-1 functions)
	//((like r2r))
	val fn1 = ranges.foldLeft(List[(Nonterminal,Symbol)]()){
			case (soFar:List[(Nonterminal,Symbol)],r:String) =>
		(Nonterminal(Symbol("F_{"+r+"}2"+r), 'none),Symbol(r)) :: soFar
	} ::: 
	//((like d2d))
	durations.foldLeft(List[(Nonterminal,Symbol)]()){
			case (soFar:List[(Nonterminal,Symbol)],d:String) =>
		(Nonterminal(Symbol("F_{"+d+"}2"+d), 'none),Symbol(d)) :: soFar
	}
	//--Error Checks
	assert(
		this.values.zipWithIndex.forall{ case (term:Nonterminal,id:Int) =>
			term.id == id },
		"Values array is out of order")
}



trait Rule {
	// -- Must Override --
	def apply(arg:Any):Any
	def apply(arg1:Any, arg2:Any):Any
	def arity:Int
	def head:Nonterminal
	def accepts(a:Nonterminal):Boolean
	def accepts(a:Nonterminal,b:Nonterminal):Boolean
	
	// -- Can Override --
	def validInput(w:Int) = isLex
	def setStr(str:String) = {}

	private var leftChild:Nonterminal = null
	private var rightChild:Nonterminal = null
	private def cacheRule:Unit = {
		if(leftChild != null){ return; }
		if(arity == 1){
			Nonterminal.values.foreach{ (child:Nonterminal) =>
				if(accepts(child)){
					assert(leftChild == null, "Multiple accepted inputs for rule")
					this.leftChild = child
				}
			}
		} else if(arity == 2){
			Nonterminal.values.foreach{ (left:Nonterminal) =>
				Nonterminal.values.foreach{ (right:Nonterminal) =>
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
	def left:Nonterminal = { cacheRule; return leftChild; }
	def right:Nonterminal = {
		 assert(arity == 2, "Bad arity"); cacheRule; return rightChild;
	}
	def child:Nonterminal = {
		assert(arity == 1, "Bad arity"); cacheRule; return leftChild;
	}
	def isLex:Boolean = (arity == 1 && child.isPreterminal)

	def signature:String = {
		val children:String = if(arity == 1){
				val a:Array[Nonterminal] = Nonterminal.values.filter( v => accepts(v) ).toArray
				a(0).toString
			} else {
				var str = "<unknown>"
				Nonterminal.values.foreach( v1 => {
					Nonterminal.values.foreach( v2 => {
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
		out:Nonterminal,
		in:Nonterminal,
		fn:Any=>Any ) extends Rule {
	private var checkValid:Int=>Boolean = (i:Int) => true
	private var str:String = null; override def setStr(str:String) = this.str=str;
	override def apply(arg:Any):Any = fn(arg)
	override def apply(arg1:Any,arg2:Any) 
		= throw fail("binary apply to unary rule")
	override def validInput(w:Int):Boolean = isLex && checkValid(w)
	def arity:Int = 1
	def head:Nonterminal = out
	def accepts(a:Nonterminal):Boolean = a == in
	def accepts(a:Nonterminal,b:Nonterminal):Boolean = false
	def ensureValidity(fn:Int=>Boolean):UnaryRule = { checkValid = fn; this }
	override def toString:String = if(str != null){
			str
		} else {
			""+out+{if(in.isPreterminal) "["+this(0)+"]" else ""}+"->"+in
		}
}
case class BinaryRule(
		out:Nonterminal,
		in1:Nonterminal,
		in2:Nonterminal,
		fn:(Any,Any)=>Any) extends Rule {
	private var str:String = null; override def setStr(str:String) = this.str=str;
	override def apply(arg:Any)
		= throw fail("unary apply to binary rule")
	override def apply(arg1:Any,arg2:Any):Any = fn(arg1,arg2)
	def arity:Int = 2
	def head:Nonterminal = out
	def accepts(a:Nonterminal):Boolean = false
	def accepts(a:Nonterminal,b:Nonterminal):Boolean = (a == in1) && (b == in2)
	override def toString:String = if(str != null){
			str
		} else {
			""+out+"->"+in1+","+in2
		}
}

object Grammar {
	case class NIL()

	val DOW_STR = Array[String]("Mon:S","Tue:S","Wed:S","Thu:S","Fri:S",
		"Sat:S","Sun:S")
	val MOY_STR = Array[String]("Jan:S","Feb:S","Mar:S","Apr:S","May:S",
		"Jun:S","Jul:S","Aug:S","Sep:S","Oct:S","Nov:S","Dec:S")
	val QOY_STR = Array[String]("Q1:S","Q2:S","Q3:S","Q4:S")
	val SEASON_STR = Array[String]("SP:S","SU:S","FA:S","WI:S")
	val TOD_STR = Array[String]("MO:S","AF:S","EV:S","NI:S")

	//(ranges)
	val ranges = List[(Range,String)](
		(PAST,"PAST:R"),(FUTURE,"FUTURE:R"),
		(YESTERDAY,"YESTERDAY:R"),(TOMORROW,"TOMORROW:R"),
		(TODAY,"TODAY:R"),
		(REF,"REF:R")
		)
	//(durations)
	val durations = 
		{if(O.useTime) List[(Duration,String)]((ASEC,"Sec:D"),(AMIN,"Min:D"),
			(AHOUR,"Hour:D")) else List[(Duration,String)]()} :::
		List[(Duration,String)](
			(ADAY,"Day:D"),(AWEEK,"Week:D"),(AMONTH,"Month:D"),(AQUARTER,"Quarter:D"),
			(AYEAR,"Year:D"),(ADECADE,"Decade:D"),(ACENTURY,"Century:D")
			)
	//(sequences)
	val sequences = 
		(1 to 7).map(i=>(DOW(i).asInstanceOf[RepeatedRange]
			.dense.name(DOW_STR(i-1)),DOW_STR(i-1)) ).toList :::
		(1 to 12).map(i=>(MOY(i).asInstanceOf[RepeatedRange]
			.dense.name(MOY_STR(i-1)),MOY_STR(i-1)) ).toList :::
//		(1 to 4).map(i=>(QOY(i).asInstanceOf[RepeatedRange]
//			.dense.name(QOY_STR(i-1)),QOY_STR(i-1)) ).toList ::: 
		(1 to 4).map(i=>(SEASON(i).asInstanceOf[RepeatedRange]
			.dense.name(SEASON_STR(i-1)),SEASON_STR(i-1)) ).toList ::: 
		(1 to 4).map(i=>(TOD(i).asInstanceOf[RepeatedRange]
			.dense.name(TOD_STR(i-1)),TOD_STR(i-1)) ).toList ::: 
//		{if(O.useTime) List[(Sequence,String)]( //TODO uncomment this
//			(SEC,"Sec:S"),(MIN,"Min:S"),
//			(HOUR,"Hour:S")) else List[(Sequence,String)]()} :::
		List[(Sequence,String)](
			(DAY,"Day:S"),(WEEK,"Week:S"),(MONTH,"Month:S"),(QUARTER,"Quarter:S"),
			(YEAR,"Year:S")
			) :::
		Nil
	
	private val NAMED_RULES:Array[(Rule,String)] = {
		def hack[A,Z](fn:A=>Z):Any=>Any = fn.asInstanceOf[Any=>Any]
		def hack2[A,B,Z](fn:(A,B)=>Z):(Any,Any)=>Any 
			= fn.asInstanceOf[(Any,Any)=>Any]
		var rtn = List[(Rule,String)]()
		//--Lex
		rtn = rtn ::: ranges.map{ case (r:Range,s:String) => 
			(UnaryRule(Nonterminal('Range), 
				Nonterminal('Word), hack((w:Int) => r)), s) }
		rtn = rtn ::: durations.map{ case (d:Duration,s:String) => 
			(UnaryRule(Nonterminal('Duration), 
				Nonterminal('Word), hack((w:Int) => d)), s) }
		rtn = rtn ::: sequences.map{ case (d:Duration,s:String) => 
			(UnaryRule(Nonterminal('Sequence), 
				Nonterminal('Word), hack((w:Int) => d)), s) }
		//(nil)
		rtn = rtn ::: 
			{if(O.lexNils){
				assert(G.wordIndexer.size > 0, "Haven't initialized indexer yet")
				G.wordIndexer.map{ (word:String) =>
					(UnaryRule(Nonterminal(Symbol("NIL-"+word)), Nonterminal('Word), 
						hack((w:Int) => new NIL)), "nil-"+word)
				}.toList
			} else {
				List[(Rule,String)]((UnaryRule(Nonterminal('NIL), Nonterminal('Word), 
						hack((w:Int) => new NIL)), "nil") )
			}}
		//(numbers)
		rtn = rtn ::: List[(Rule,String)](
			(UnaryRule(Nonterminal('Number), Nonterminal('Number), 
				hack((num:Int) =>  num )),
				"NUM"),
			(UnaryRule(Nonterminal('Sequence), Nonterminal('Number), 
				hack((num:Int) =>  MOH(num) ))
				.ensureValidity( (w:Int) => w >= 0 && w < 60 ),
				"moh(n):S"),
			(UnaryRule(Nonterminal('Sequence), Nonterminal('Number), 
				hack((num:Int) =>  HOD(num) ))
				.ensureValidity( (w:Int) => w >= 0 && w < 24 ),
				"hod(n):S"),
			(UnaryRule(Nonterminal('Sequence), Nonterminal('Number), 
				hack((num:Int) =>  DOW(num) ))
				.ensureValidity( (w:Int) => w >= 1 && w <= 7 ),
				"dow(n):S"),
			(UnaryRule(Nonterminal('Sequence), Nonterminal('Number), 
				hack((num:Int) =>  DOM(num) ))
				.ensureValidity( (w:Int) => w >= 1 && w <= 31 ),
				"dom(n):S"),
			(UnaryRule(Nonterminal('Sequence), Nonterminal('Number), 
				hack((num:Int) =>  WOY(num) ))
				.ensureValidity( (w:Int) => w >= 1 && w <= 52 ),
				"woy(n):S"),
			(UnaryRule(Nonterminal('Sequence), Nonterminal('Number), 
				hack((num:Int) =>  MOY(num) ))
				.ensureValidity( (w:Int) => w >= 1 && w <= 12 ),
				"moy(n):S"),
			(UnaryRule(Nonterminal('Sequence), Nonterminal('Number), 
				hack((num:Int) =>  QOY(num) ))
				.ensureValidity( (w:Int) => w >= 1 && w <= 4 ),
				"qoy(n):S"),
			(UnaryRule(Nonterminal('Sequence), Nonterminal('Number), 
				hack((num:Int) =>  YOC(num) ))
				.ensureValidity( (w:Int) => w >= 0 && w < 100 ),
				"yoc(n):S"),
			(UnaryRule(Nonterminal('Sequence), Nonterminal('Number), 
				hack((num:Int) =>  DOC(num) ))
				.ensureValidity( (w:Int) => w >= 0 && w < 10 ),
				"doc(n):S"),
			(UnaryRule(Nonterminal('Sequence), Nonterminal('Number), 
				hack((num:Int) =>  YOD(num) ))
				.ensureValidity( (w:Int) => w >= 0 && w < 10 ),
				"yod(n):S"),
			(UnaryRule(Nonterminal('Range), Nonterminal('Number), 
				hack((num:Int) =>  THEYEAR(num) )),
				"year(n):R"),
			(UnaryRule(Nonterminal('Range), Nonterminal('Number), 
				hack((num:Int) =>  CENTURY(num) ))
				.ensureValidity( (w:Int) => w > -100 && w < 100 ),
				"century(n):R")
			)

		//--Arity 2 Functions
		//(util)
		case class F2Info[A,B](
			fn:(_<:A,_<:B)=>_<:A,name:String,validA:List[Symbol],validB:List[Symbol])
		//(define)
		val function2 = List[F2Info[Temporal,Temporal]](
			F2Info(shiftLeft,"shiftLeft",List('R,'S),List('D)),        //last/ago
			F2Info(shiftRight,"shiftRight",List('R,'S),List('D)),      //next/from now
			F2Info(shrinkBegin,"shrinkBegin",List('R,'S),List('D)),    //first
			F2Info(shrinkBegin,"shrinkEnd",List('R,'S),List('D)),      //last
			F2Info(catLeft,"catLeft",List('R),List('D)),               //past
			F2Info(catRight,"catRight",List('R),List('D))              //coming
//			F2Info(cons,"cons",List('R,'S),List('R,'S))                //from...until
		)
		//(intro)
		function2.foreach{ (info:F2Info[Temporal,Temporal]) =>
			//(for every argA...)
			rtn = rtn ::: info.validA.foldLeft(List[(Rule,String)]()){ 
					case (soFarOuter:List[(Rule,String)],a:Symbol) =>
				//(for every argB...)
				soFarOuter ::: info.validB.foldLeft(List[(Rule,String)]()){
						case (soFarInner:List[(Rule,String)],b:Symbol) =>
					//(create rule)
					val rule = (UnaryRule(Nonterminal("F_{"+a.name+b.name+"}2"+a.name),
						Nonterminal('Word),
						hack((w:Int) => info.fn)),
						info.name+"$(-:"+a.name+",-:"+b.name+"):"+a.name+"$" )
					//(append rule)
					rule :: soFarInner
				}
			}
		}
		//(apply)
		rtn = rtn ::: {
			Nonterminal.fn2.foldLeft(List[(Rule,String)]()){
					case (soFar:List[(Rule,String)],(fn:Nonterminal,a:Symbol,b:Symbol)) =>
//				//(consume A on the left)
//				( BinaryRule(
//					Nonterminal(Symbol("F_"+b.name+"2"+a.name)),               //head
//					Nonterminal.fromShort(a),                                  //left
//					fn,                                                        //right
//					hack2((a:Temporal,fn:(Temporal,Temporal)=>Temporal) =>     //function
//						fn(a,_:Temporal)) ),
//				"$f(x:"+a.name+",-:"+b.name+"):"+a.name+"$") ::              //name
//				//(consume A on the right)
//				( BinaryRule(
//					Nonterminal(Symbol("F_"+b.name+"2"+a.name)),               //head
//					fn,                                                        //left
//					Nonterminal.fromShort(a),                                  //right
//					hack2((fn:(Temporal,Temporal)=>Temporal,a:Temporal) =>     //function
//						fn(a,_:Temporal)) ),
//				"$f(x:"+a.name+",-:"+b.name+"):"+a.name+"$") ::              //name
				//(consume B on the left)
				{( BinaryRule(
					Nonterminal(Symbol("F_{"+a.name+"}2"+a.name)),               //head
					Nonterminal.fromShort(b),                                  //left
					fn,                                                        //right
					hack2((b:Temporal,fn:(Temporal,Temporal)=>Temporal) =>     //function
						fn(_:Temporal,b)) ),
				"$f(-:"+a.name+",x:"+b.name+"):"+a.name+"$") ::              //name
				//(consume B on the right)
				( BinaryRule(
					Nonterminal(Symbol("F_{"+a.name+"}2"+a.name)),               //head
					fn,                                                        //left
					Nonterminal.fromShort(b),                                  //right
					hack2((fn:(Temporal,Temporal)=>Temporal,b:Temporal) =>     //function
						fn(_:Temporal,b)) ),
				"$f(-:"+a.name+",x:"+b.name+"):"+a.name+"$") ::              //name
				Nil} ::: soFar
			}
		}
		//(ref augmented apply)
		rtn = rtn ::: Nonterminal.fn2.filter {
				case (fn:Nonterminal,a:Symbol,b:Symbol) => 
					a == 'R || b == 'R
			}.foldLeft(List[(Rule,String)]()){
				case (soFar:List[(Rule,String)],(fn:Nonterminal,a:Symbol,b:Symbol))=>
			{if(b == 'R){
				//(consume A on the left -- B is Range)
				( BinaryRule(
					Nonterminal.fromShort(a),                                  //head
					Nonterminal.fromShort(a),                                  //left
					fn,                                                        //right
					hack2((a:Temporal,fn:(Temporal,Range)=>Temporal) => {      //function
						fn(a,REF)}) ),
				"$r:"+a.name+"$") ::                                         //name
				//(consume A on the right -- B is Range)
				( BinaryRule(
					Nonterminal.fromShort(a),                                  //head
					fn,                                                        //left
					Nonterminal.fromShort(a),                                  //right
					hack2((fn:(Temporal,Range)=>Temporal,a:Temporal) => {      //function
						fn(a,REF)}) ),
				"$r:"+a.name+"$") ::                                         //name
				Nil
			} else {List[(Rule,String)]()} :::
			{if(a == 'R){
				//(consume B on the left -- A is Range)
				( BinaryRule(
					Nonterminal.fromShort(a),                                  //head
					Nonterminal.fromShort(b),                                  //left
					fn,                                                        //right
					hack2((b:Temporal,fn:(Range,Temporal)=>Temporal) => {      //function
						fn(REF,b)}) ),
				"$r:"+a.name+"$") ::                                         //name
				//(consume B on the right -- A is Range)
				( BinaryRule(
					Nonterminal.fromShort(a),                                  //head
					fn,                                                        //left
					Nonterminal.fromShort(b),                                  //right
					hack2((fn:(Range,Temporal)=>Temporal,b:Temporal) => {      //function
						fn(REF,b)}) ),
				"$r:"+a.name+"$") ::                                         //name
				Nil
			} else { Nil } }} ::: soFar
		}
		
		//--Arity 1 Functions
		//(util)
		case class F1Info[A](
			fn:(_<:A)=>_<:A,name:String,validA:List[Symbol])
		//(define)
		val function1 = List[F1Info[Temporal]](
			F1Info(fuzzify,"fuzzify",List('D)),                  //around
			F1Info(move(_:Sequence,-1L),"moveLeft1",List('S)),   //last [sequence]
			F1Info(move(_:Sequence,1L),"moveRight1",List('S))    //next [sequence]
		)
		//(intro)
		function1.foreach{ (info:F1Info[Temporal]) =>
			//(for every argA...)
			rtn = rtn ::: info.validA.foldLeft(List[(Rule,String)]()){ 
					case (soFarOuter:List[(Rule,String)],a:Symbol) =>
				//(create rule)
				val rule = (UnaryRule(Nonterminal("F_{"+a.name+"}2"+a.name),
					Nonterminal('Word),
					hack((w:Int) => info.fn)),
					info.name+"$(-:"+a.name+"):"+a.name+"$" )
				//(append rule)
				rule :: soFarOuter
			}
		}
		//(apply)
		rtn = rtn ::: {
			Nonterminal.fn1.foldLeft(List[(Rule,String)]()){
					case (soFar:List[(Rule,String)],(fn:Nonterminal,a:Symbol)) =>
				//(consume A on the left)
				{( BinaryRule(
					Nonterminal.fromShort(a),                                  //head
					Nonterminal.fromShort(a),                                  //left
					fn,                                                        //right
					hack2((a:Temporal,fn:(Temporal)=>Temporal) => fn(a)) ),    //function
				"$x:"+a.name+"$") ::                                         //name
				//(consume A on the right)
				( BinaryRule(
					Nonterminal.fromShort(a),                                  //head
					fn,                                                        //left
					Nonterminal.fromShort(a),                                  //right
					hack2((fn:(Temporal)=>Temporal,a:Temporal) => fn(a)) ),    //function
				"$x:"+a.name+"$") ::                                         //name
				Nil} ::: soFar
			}
		}
		
		//--Multiply Duration
		rtn = rtn ::: List[(Rule,String)](
			(BinaryRule(Nonterminal('Duration), 
				Nonterminal('Duration), Nonterminal('Number), hack2(
				(d:Duration,n:Int) => d*n
				)), "D*n"),
			(BinaryRule(Nonterminal('Duration), 
				Nonterminal('Number), Nonterminal('Duration), hack2(
				(n:Int,d:Duration) => d*n
				)), "n*D")
			)

		//--Intersect
		rtn = rtn ::: Nonterminal.ranges.foldLeft(List[(Rule,String)]()) {
				case (soFar:List[(Rule,String)],rA:String) =>
			Nonterminal.ranges.foldLeft(List[(Rule,String)]()){
					case (soFarInner:List[(Rule,String)],rB:String) =>
				val head = {if(rA == "R" || rB == "R") "R" else rA}
				(BinaryRule(Nonterminal.fromShort(head), 
					Nonterminal.fromShort(rA),Nonterminal.fromShort(rB),
					hack2( (r1:Range,r2:Range) => r1 ^ r2)), 
				"$a + b:"+head+"$") :: soFarInner
			} ::: soFar
		}

		//--NIL Identities
		rtn = rtn ::: Nonterminal.values.filter{ (x:Nonterminal) =>
					!x.isNil && x != Nonterminal('ROOT) }.
				foldLeft(List[(Rule,String)]()){
				case (soFar:List[(Rule,String)],term:Nonterminal) => 
			//(get possible nils)
			val nilList:List[Nonterminal] =
				{if(O.lexNils){
					G.wordIndexer.map{ (word:String) => Nonterminal(Symbol("NIL-"+word)) }
				} else {
					List[Nonterminal](Nonterminal('NIL))
				}}.toList
			//(add identities)
			nilList.foldLeft(List[(Rule,String)]()){ 
					case (lst:List[(Rule,String)],nil:Nonterminal) =>
				(BinaryRule(term,term,nil,
					hack2( (x:Any,n:NIL) => x)),"$x:"+Nonterminal.name(term)+"$") ::
				(BinaryRule(term,nil,term,
					hack2( (n:NIL,x:Any) => x)),"$x:"+Nonterminal.name(term)+"$") :: lst
			} ::: soFar
		}

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

		//--ROOT
		rtn = rtn ::: List[UnaryRule](
			UnaryRule(Nonterminal('ROOT), Nonterminal('Range), 
				hack((r:Range) => r)),
			UnaryRule(Nonterminal('ROOT), Nonterminal('Duration), 
				hack((d:Duration) => d)),
			UnaryRule(Nonterminal('ROOT), Nonterminal('Sequence), 
				hack((s:Range) => s)) //note: range
			) ::: { if(O.allowPartialTime)
				List[UnaryRule]( 
					UnaryRule(Nonterminal('ROOT), Nonterminal('F_R2R), 
						hack((fn:Range=>Range) => fn))
				) else List[UnaryRule]() }
		//--Return
		rtn.toArray
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

	case class Closure(head:Nonterminal,child:Nonterminal,rules:Array[Int])

	private def computeClosures(raw:Array[Rule]):Array[Closure] = {
		//--Construct Graph
		case class Node(head:Nonterminal,var neighbors:List[(Node,Int)]){
			def this(head:Nonterminal) = this(head,List[(Node,Int)]())
			def addNeighbor(n:Node,rid:Int) = { neighbors = (n,rid)::neighbors }
			def search(seen:Array[Boolean],backtrace:List[Int],
					tick:(Nonterminal,List[Int])=>Any):Unit = {
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
		val graph = Nonterminal.values.toArray.map{ new Node(_) }
		UNARIES.foreach{ case (r,rid) => 
			assert(r.head != Nonterminal('Word), "Unary headed by a Word")
			if(!r.child.isPreterminal){ //don't add lex rules
				graph(r.head.id).addNeighbor(graph(r.child.id),rid) 
			}
		}
		//--Search Graph
		var closures = List[Closure]()
		graph.foreach{ (start:Node) => 
			start.search(new Array[Boolean](graph.length), List[Int](),
				(child:Nonterminal,backtrace:List[Int]) => {
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
		if(U.isNum(words(i))){
			assert(nums(i) != Int.MinValue, 
				"Returning number that was not set: " + U.w2str(words(i)))
			nums(i).toString
		} else {
			U.w2str(words(i))
		}
	}
	def foreach(fn:(Int,Int)=>Any) = words.zip(pos).foreach(Function.tupled(fn))
	def length:Int = words.length
	def gloss:String 
		= words.zipWithIndex.map{ case (w:Int,i:Int) =>
			if(U.isNum(w)){ U.w2str(w)+"["+nums(i).toString+"]" }
			else{ U.w2str(w) } } mkString " "
	override def toString:String = U.sent2str(words)
}
case class Feedback(ref:Temporal,grounding:Time,
		correct:Array[(Int,Int,Double)],
		incorrect:Array[(Int,Int,Double)],
		isCorrect:Boolean) {
	def bestScore = correct(0)._3
	def hasCorrect:Boolean = correct.length > 0
	def bestIndex:Int = correct(0)._1
	def bestOffset:Int = correct(0)._2
	def tiedBest:Array[(Int,Int)] = {
		correct.filter{ case (index,offset,score) => score == bestScore }.map{ 
			case (index:Int,offset:Int,score:Double) => (index,offset) }
	}
	def wasWrong:Boolean = !isCorrect
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
trait ParseTree extends Tree[Nonterminal] {
	//<<Common Methods>>
	private def cleanParseString(
			headType:Symbol,
			indent:Int,b:StringBuilder,sent:Sentence,i:Int):Int = {
		for(i <- 0 until indent){ b.append("  ") }
		val head:String = headType match {
				case 'String => headString
				case 'Probability => {
					val prob = headProbability
					assert(!prob.isNaN, "Rule has NaN probability")
					assert(prob >= 0.0 && prob <= 1.0, "Rule has malformed probability")
					G.df.format(prob)
				}
			}
		if(isLeaf){
			//--Case: Leaf
			val tail = leafString(sent,i)
			if(tail != null && !tail.equals("")){
				b.append("(").append(head.replaceAll(" ","_")).append(" ")
				b.append(tail.replaceAll(" ","_").replaceAll("'","\\'")).append(")")
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
			b.append("(").append(head.replaceAll(" ","_"))
			for(i <- 0 until ch.length) {
				val child:ParseTree = ch(i)
				b.append("\n")
				retI = child.cleanParseString(headType,indent+1,b,sent,retI)
			}
			b.append(")")
			retI
		}
	}
	def asParseString(sent:Sentence):String = {
		val b = new StringBuilder
		cleanParseString('String,0,b,sent,0)
		b.toString
	}
	def asParseProbabilities(sent:Sentence):String = {
		val b = new StringBuilder
		cleanParseString('Probability,0,b,sent,0)
		b.toString
	}
	def lexRules:Array[Int] = {
		var terms = List[Int]()
		traverse( 
			(rid:Int,nilTag:Option[Int]) => {}, 
			(rid:Int,w:Int) => { terms = rid :: terms } )
		terms.reverse.toArray
	}
	def trim(sent:Sentence):Array[Int] = {
		var words = List[Int]()
		var tags = List[Int]()
		traverse( 
			(rid:Int,nilTag:Option[Int]) => {}, 
			(rid:Int,w:Int) => { 
				tags = rid :: tags 
				words = sent.words(w) :: words 
			} )
		def isNil(wordTag:(Int,Int)) = { 
			val (w,t) = wordTag
			Grammar.RULES(t).head.isNil 
		}
		words.zip(tags)
			.dropWhile{isNil(_)}.reverse.dropWhile{ isNil(_) }.map{_._1}.toArray
	}
	//<<Possible Overrides>>
	def headString:String = head.toString
	def headProbability:Double = 1.0
	def leafString(sent:Sentence,index:Int):String = sent(index)
	def maxDepth:Int = throw fail() //TODO implement something reasonable here
	//<<Overrides>>
	override def children:Array[ParseTree]
	def evaluate(sent:Sentence):(Nonterminal,Temporal,Double)
	def traverse(ruleFn:(Int,Option[Int])=>Any,lexFn:(Int,Int)=>Any):Unit 
		//^ ruleFn: (rid,nilTag)=>Any, lexFn: (rid,w)=>Any
}

//-----
// Parse
//-----
case class Parse(value:Temporal,logProb:Double){
	var tree:Option[String] = None
	var probs:Option[String] = None
	def this(value:Temporal) = this(value,0.0)

	def scoreFrom(gold:Temporal,ground:GroundedRange
			):Iterator[((Duration,Duration),Double,Int)] = {
		val INF = (Duration.INFINITE,Duration.INFINITE)
		def diff(gold:Temporal,guess:Temporal,second:Boolean):(Duration,Duration) 
				= (gold,guess) match {
			//--Immediate Invalids
			//(case: unks)
			case (gold:UnkTime,guess:Temporal) => INF
			case (gold:Temporal,guess:NoTime) => INF
			case (gold:Temporal,guess:Sequence) =>
				throw fail("Distribution returned a sequence: " + guess)
			case (gold:Sequence,guess:Temporal) =>
				throw fail("Gold is a sequence: " + gold)
			//(case: type errors)
			case (gold:FuzzyDuration,guess:GroundedDuration) => INF
			case (gold:GroundedDuration,guess:FuzzyDuration) => INF
			//--Valid
			//(case: durations)
			case (gold:FuzzyDuration,guess:FuzzyDuration) => 
				if(gold.largestUnit == guess.largestUnit){(Duration.ZERO,Duration.ZERO)}
				else{ INF }
			case (gold:GroundedDuration,guess:GroundedDuration) => 
				(guess-gold,Duration.ZERO)
			//(case: grounded ranges)
			case (gold:GroundedRange,guess:GroundedRange) => 
				if(guess.norm.seconds == 0 && gold.norm.seconds == 0){
					(Duration.ZERO,guess.begin-gold.begin) //case: instant
				} else if(O.instantAsDay && gold.norm.seconds == 0){
					(guess.begin-gold.begin,guess.end-(gold.end+DAY))
				} else {
					assert(!guess.begin.equals(Time.DAWN_OF) || guess.begin==Time.DAWN_OF)
					assert(!guess.end.equals(Time.END_OF) || guess.end==Time.END_OF)
					if(guess.begin == Time.DAWN_OF && gold.begin != Time.DAWN_OF){
						INF //case: beginning is neg_infinity
					} else if(guess.end == Time.END_OF && gold.end != Time.END_OF){
						INF //case: end is pos_infinity
					} else {
						(guess.begin-gold.begin,guess.end-gold.end) //case: can subtract
					}
				}
			//--Possibly Valid
			//(case: backoffs)
			case (gold:Range,guess:GroundedRange) => 
				if(second){ INF }
				else { val gr:GroundedRange = gold(ground); diff(gr,guess,true) }
			case (gold:PartialTime,guess:Temporal) =>
				if(second){ INF }
				else { val gr:GroundedRange = gold(ground); diff(gr,guess,true) }
			//--Not Valid
			//(case: didn't catch above)
			case (gold:Duration,guess:Duration) => throw fail("case: 2 durations")
			case (gold:Range,guess:Range) => throw fail("case: 2 durations")
			//(case: type error)
			case (gold:Duration,guess:Temporal) =>  INF
			case (gold:Range,guess:Temporal) => INF
			//(case: default fail)
			case _ => throw fail("Unk (invalid?) case: gold "+gold+" guess "+guess)
		}
		//--Map Iterator
		value.distribution(ground).map{
				case (guess:Temporal,prob:Double,offset:Long) =>
			assert(O.timeDistribution != O.Distribution.Point ||
				offset == 0L ||
				prob == 0.0,
				"Time returned distribution when it shouldn't have: " 
					+ guess + " (offset=" + offset + ") [prob=" + prob + "]")
			(diff(gold,guess,false),prob,offset.toInt)
		}
	}
	
	def ground(ground:Time):Temporal
		= if(value.exists(Range(ground,ground),0)){
				value(Range(ground,ground))
			} else { new NoTime }
}

object Parse {
	def apply(parseType:Nonterminal,parseValue:Temporal,logProb:Double):Parse = {
		assert(parseType == Nonterminal('ROOT), "No parse for non-root node")
		new Parse(parseValue,logProb)
	}
	def apply(parseType:Nonterminal,parseValue:Temporal):Parse
		= apply(parseType,parseValue,0.0)
	def apply(parseValue:Temporal):Parse
		= apply(Nonterminal('ROOT),parseValue,0.0)
}

//-----
// Parse Traits
//-----
trait Parser extends OtherSystem {
	def report:Unit = {}
	def beginIteration(iter:Int,feedback:Boolean,data:DataStore):Unit = {}
	def endIteration(iter:Int,feedback:Boolean,data:DataStore):Unit = {}
	def parse(iter:Int, sent:Sentence, feedback:Boolean,sid:String
		):(Array[Parse],Feedback=>Any)

	def cycle(data:DataStore,iters:Int,feedback:Boolean):Array[Score] = {
		(1 to iters).map( (i:Int) => {
			startTrack("Iteration " + i)
			//(begin)
			beginIteration(i,feedback,data)
			//(run)
			val score = data.eachExample{ (sent:Sentence,id:Int) => 
				parse(i, sent, feedback, data.name+":"+id)
			}
			//(end)
			endIteration(i,feedback,data)
			//(score)
			log(FORCE,BOLD,YELLOW,""+score)
			log(FORCE,YELLOW,""+score.reportK)
			if(O.printFailures){
				startTrack("failures")
				score.reportFailures{ (str:String) => log(FORCE,str) }
				endTrack("failures")
			}
			if(i != iters){ score.releaseResults } //release some memory
			endTrack("Iteration " + i)
			score
		}).toArray
	}

	def run(data:Data,iters:Int):(Array[Score],Score) = {
		startTrack("Training ("+data.train.name+")")
		val train = cycle(data.train,iters, true)
		endTrack("Training ("+data.train.name+")")
		startTrack("Testing ("+data.eval.name+")")
		val test = cycle(data.eval, 1, false)(0)
		endTrack("Testing ("+data.eval.name+")")
		startTrack("Parser State")
		report
		endTrack("Parser State")
		(train,test)
	}
	def getTimex(input:SystemInput):Option[SystemOutput] = {
		//--Create Sentence
		val sent = Sentence(
			input.timex.tid,
			input.timex.words(true),
			input.timex.pos(true),
			input.timex.nums)
		//--Run Parser
		val saveBeam = O.beam
		O.beam = 10
		val (parses,feedbackFn):(Array[Parse],Feedback=>Any)
			= parse(1,sent,false,input.timex.index.toString)
		O.beam = saveBeam
		//--Digest Result
		//(get best parse)
		val parsesWithTime:Array[Parse] 
			= parses.dropWhile{ (p:Parse) => p.value.isInstanceOf[NoTime] }
		if(parsesWithTime.length == 0){
			return None
		} else {
			val grounded = parsesWithTime(0).value(new Time(input.ground))
			grounded match {
				case (gr:GroundedRange) =>
					//((case: grounded range))
					Some(SystemOutput(
						Some("DATE"),
						Some(JodaTimeUtils.timexDateValue(gr.begin.base,gr.end.base))))
				case (d:GroundedDuration) =>
					//((case: duration))
					Some(SystemOutput(
						Some("DURATION"),
						Some(JodaTimeUtils.timexDurationValue(d.base,false))))
				case (d:FuzzyDuration) =>
					//((case: approx. duration))
					Some(SystemOutput(
						Some("DURATION"),
						Some(JodaTimeUtils.timexDurationValue(d.interval.base,true))))
				case _ =>
					throw new IllegalStateException("Unknown type: "+grounded)
			}
		}
	}
	def name:String = "MyTime"
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
	// Values
	//-----
	//(guessed parses)
	private case class GuessInfo
			(sid:String,parse:ParseTree,score:Double,sent:Sentence){
		var correct=true; def wrong:GuessInfo = {correct = false; this}
		var feedback:Feedback=null
		def feedback(f:Feedback):GuessInfo = { feedback=f; this }
		def parseVal:Parse = Parse(Nonterminal('ROOT),parse.evaluate(sent)._2)
		def isFailedParse:Boolean = { parse == null }
	}
	private var corrects = List[GuessInfo]()
	private var guesses = List[GuessInfo]()
	private var timesToNormalize = List[()=>Any]()

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
	val CKY_NIL:Array[CkyRule] = CKY_LEX
		.filter{ (rule:CkyRule) => rule.head.isNil }
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
	
	
	//(learning)
	private var (pRuleGivenHead,rid2RuleGivenHeadIndices)
			:(Array[Multinomial[Int]],Array[(Int,Int)]) = {
		val mapping = (0 until RULES.length).map{ x => (-1,0) }.toArray
		val distributions:Array[Multinomial[Int]]
				= Nonterminal.values.map{ (head:Nonterminal) =>
			//((create structures))
			RULES.zipWithIndex.filter{ _._1.head == head }.map{ _._2 }
					.zipWithIndex.foreach{ case (rid:Int,index:Int) =>
				mapping(rid) = (head.id,index)
			}
			val size = RULES.filter{ _.head == head }.length
			val mult:Multinomial[Int] = new Multinomial[Int](U.intStore(size))
			//((initialize))
			(0 until size).foreach{ case (i:Int) =>
				O.initMethod match {
					case O.InitType.uniform => mult.incrementCount(i,1.0)
					case O.InitType.random => mult.incrementCount(i,U.rand)
				}
			}//((return))
			(head.id,mult)
		}.toArray.sortBy( _._1 ).map{ _._2 }
		(distributions,mapping)
	}
	private var ruleESS:Array[ExpectedSufficientStatistics[Int,Multinomial[Int]]]
		= pRuleGivenHead.map{ _.newStatistics(O.rulePrior) }

	private var (pWordGivenRule,rid2WordGivenRuleIndices)
			:(Array[Multinomial[Int]],Array[Int]) = {
		val mapping = (0 until RULES.length).map{ x => -1 }.toArray
		val distributions:Array[Multinomial[Int]]
				= RULES.zipWithIndex
						.filter( _._1.isLex)
						.zipWithIndex
						.map{ case ((r:Rule,rid:Int),termId) =>
					mapping(rid) = termId
					val dist = new Multinomial[Int](U.intStore(G.W+1))
					O.initMethod match {
						case O.InitType.uniform => dist.initUniform
						case O.InitType.random => dist.initRandom
					}
				}
		(distributions,mapping)
	}
	private var lexESS:Array[ExpectedSufficientStatistics[Int,Multinomial[Int]]]
		= pWordGivenRule.map{ _.newStatistics(O.lexPrior) }
	
	

	//-----
	// Subclasses
	//-----
	case class CkyRule(
			arity:Int,
			head:Nonterminal,
			childOrNull:Nonterminal,
			rids:Array[Int]) {
		def rid:Int = {
			if(rids.length > 1){ 
				throw new IllegalArgumentException("multiple rules")
			}
			rids(0)
		}
		def rule:Rule = (RULES(rid))
		def rules:Array[Rule] = rids.map{ RULES(_) }
		def child:Nonterminal = {
			assert(arity == 1, "child only defined for unary rules")
			assert(childOrNull != null, "something went very wrong")
			childOrNull
		}
		def numChildren:Int = rids.length
		def isLex:Boolean = rids.length == 1 && rule.isLex
		def validInput(w:Int):Boolean = rule.validInput(w)
		override def toString:String = "<" + rules.mkString(", ") + ">"
	}

	class ChartElem(
			var logScore:Double, 
			var term:CkyRule, 
			var left:ChartElem,
			var right:ChartElem,
			var index:Option[Int]) extends ParseTree {
		// -- CKY Properties --
		def this(index:Option[Int]) 
			= this(Double.NegativeInfinity,null,null,null,index)
		def apply(logScore:Double,term:CkyRule,left:ChartElem,right:ChartElem
				):ChartElem = {
			assert(logScore <= 0.0, "Setting ChartElem with bad log score: "+logScore)
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
		override def head:Nonterminal = {
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
				):(Int,(Nonterminal,Any) ) = {
			assert(term != null, "evaluating null rule")
			if(term.arity == 1) {
				//(case: unary rule)
				assert(right == null, "binary rule on closure ckyI")
				var (childI,(childType,childValue)) = {
					if(isLeaf){ //<--Base Case (leaf node)
						if(U.isNum(sent.words(i))){
							//(case: number)
							(i+1,(Nonterminal('Number),sent.nums(i)))
						} else {
							//(case: word)
							(i+1,(Nonterminal('Word),sent.words(i)))
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
				val r:Rule = term.rule
				assert(r.arity == 2, "non-closure unary")
				val (leftI,(leftType,leftValue)) = left.evaluateHelper(sent,i)
				val (rightI,(rightType,rightValue)) = right.evaluateHelper(sent,leftI)
				val childValue = r(leftValue,rightValue)
				assert(childValue != null, "null value returned")
				(rightI,(r.head,childValue))
			} else {
				throw new IllegalStateException("Invalid cky term")
			}
		}
		private var evalCache:(Nonterminal,Temporal,Double) = null
		override def evaluate(sent:Sentence):(Nonterminal,Temporal,Double) = {
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
				ruleFn:(Int,Option[Int])=>Any,lexFn:(Int,Int)=>Any,up:()=>Any):Int = {
			//--Traverse
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
					term.rids.foreach{ (rid:Int) => 
						stackDepth+=1; 
						ruleFn(rid,None) 
					}
					left.traverseHelper(i,ruleFn,lexFn,up) //return
				}
			}else if(term.arity == 2){
				term.rids.foreach{ (rid:Int) => 
					stackDepth+=1; 
					ruleFn(rid,None) 
				}
				val leftI = left.traverseHelper(i,ruleFn,lexFn,up)
				right.traverseHelper(leftI,ruleFn,lexFn,up) //return
			}else{
				throw new IllegalStateException("Invalid cky term")
			}
			//(pop stack and return)
			(0 until stackDepth).foreach{ (rid:Int) => up() }
			pos
		}
		override def traverse(
				ruleFn:(Int,Option[Int])=>Any,
				lexFn:(Int,Int)=>Any):Unit = {
			traverseHelper(0,ruleFn,lexFn,()=>{})
		}
		private def cleanParseString(headType:Symbol,sent:Sentence):String = {
			val b = new StringBuilder
			//(clean string)
			def clean(str:String) = {
				"\"" + str.replaceAll("\"","\\\\\"").replaceAll("'","\\\\'") + "\""
			}
			//(traverse)
			traverseHelper(0,
				(rid:Int,nilTag:Option[Int]) => {
					val head:String = headType match {
						case 'String => clean(Grammar.RULES_STR(rid))
						case 'Probability => {
							val (headID,multID) = rid2RuleGivenHeadIndices(rid)
					 		G.df.format(pRuleGivenHead(headID).prob(multID))
						}
					}
					b.append("(").append(head).append(" ")
				},
				(rid:Int,w:Int) => {
					val head:String = headType match {
						case 'String => clean(Grammar.RULES_STR(rid))
						case 'Probability =>
							try {
								G.df.format(pWordGivenRule(rid2WordGivenRuleIndices(rid))
									.prob(sent.words(w)))
							} catch {
								case (e:ArrayIndexOutOfBoundsException) => "N/A"
							}
					}
					b.append("( ").append(head).append(" ").
						append(clean(sent(w))).append(" ) ")
				},
				() => {
					b.append(") ")
				})
			b.toString
		}
		override def asParseString(sent:Sentence):String 
			= cleanParseString('String,sent)
		override def asParseProbabilities(sent:Sentence):String
			= cleanParseString('Probability,sent)
		override def headProbability:Double = {
			val subProbs:Double = 
				if(term.isLex) { 0.0 }
				else if(term.arity == 1) { left.logScore }
				else { left.logScore + right.logScore }
			val thisProb:Double = logScore - subProbs
			math.exp(thisProb)
		}
		override def maxDepth:Int = {
			var maxDepth = 0
			var depth = 0
			traverseHelper(0,
				(rid:Int,nilTag:Option[Int]) => {
					depth += 1
					maxDepth = math.max(depth,maxDepth) 
				},
				(rid:Int,w:Int) => { },
				() => { depth -= 1 })
			maxDepth+1 // +1 for lex terms
		}
		def deepclone:ChartElem = {
			val leftClone = if(left == null) null else left.deepclone
			val rightClone = if(right == null) null else right.deepclone
			new ChartElem(logScore,term,leftClone,rightClone,index)
		}
		// -- Object Properties --
		override def clone:ChartElem = {
			new ChartElem(logScore,term,left,right,index)
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
				term + " -> (" + children.map{ _.head }.mkString(", ") + ")"
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
			lazyNextFn(Unit)
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
				override def toString:String = "DataSource(score="+score+
					",source="+source+",leftI="+leftI+",rightI="+rightI+")"
			}
			val pq = new PriorityQueue[DataSource]
			val seen = new HashSet[Int]
			val seenUnary = new HashSet[Int]
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
					//(score from cfg)
					val cfgScore:Double = if(right == null) {
							seenUnary( source*capacity + lI ) = true
							left(lI).logScore+score(left(lI),null)
						} else {
							seen(	source * capacity * capacity + 
								        lI * capacity +
												rI			) = true
							left(lI).logScore+right(rI).logScore+score(left(lI),right(rI))
						}
						pq.enqueue( DataSource(cfgScore,source,lI,rI) )
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
						assert(datum.score <= 0, "Log probability > 0: " + datum.score)
						values(length)(datum.score,rule,left(lI))
					} else {
						assert(datum.score <= 0, "Log probability > 0: " + datum.score)
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
	
	val makeChart:Long=>((Int,Int)=>Chart) = {
		val chartMap = new HashMap[Long,(Int,Int)=>Chart]
		(thread:Long) =>
			if(chartMap.contains(thread)){
				chartMap(thread)
			} else {
				var largestChart = new Chart(0)
				var largestBeam = 0
				val fn = (inputLength:Int,inputBeam:Int) => {
					//--Make Chart
					val chart = if(inputLength > largestChart.length || 
							inputBeam > largestBeam){ 
						val len = math.max(inputLength, largestChart.length)
						val beam = math.max(inputBeam, largestBeam)
						//(create)
						largestChart = (0 until len).map{ (start:Int) =>            //begin
							assert(len-start > 0,"bad length end on start "+start+" len "+len)
							(0 until (len-start)).map{ (length:Int) =>                //length
								assert(Nonterminal.values.size > 0, "bad rules end")
								(0 to 1).map{ (arity:Int) =>                            //arity
									Nonterminal.values.map{ (term:Nonterminal) =>         //rules
										assert(beam > 0, "bad kbest end")
										new BestList((0 until beam).map{ (kbestItem:Int) =>  //kbest
											new ChartElem(
												if(length == 0 && term.isNil) Some(start)
												else None)
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
			chartMap(thread) = fn
			fn
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
		assert(head < Nonterminal.values.size, "Chart access error: bad head: " + head)
		assert(t == 0 || t == 1, "must be one of UNARY/BINARY")
		//(access)
		chart(begin)(end-begin-1)(t)(head)
	}
	def lex(chart:Chart,elem:Int,head:Int,t:Int=BINARY):BestList = {
		//(asserts)
		assert(elem >= 0, "Chart access error: negative value: " + elem)
		assert(head >= 0, "Chart access error: bad head: " + head)
		assert(head < Nonterminal.values.size, "Chart access error: bad head: " + head)
		chart(elem)(0)(t)(head)
	}
	
	//-----
	// Learning
	//-----

	
	
	def lexLogProb(w:Int,pos:Int,rule:CkyRule):Double = {
		assert(rule.arity == 1, "Lex with binary rule")
		assert(w < G.W || w == G.UNK, "Word is out of range: " + w)
		assert(rule.isLex,"Lex probability accessed on non-lex rule")
		if(O.freeNils && rule.head.isNil){
			U.safeLn( 1.0 )
		} else {
			try {
				//(case: get word prob)
				if(O.includeRuleInLexProb){ ruleLogProb(rule) } else { 0.0 } + 
					U.safeLn( pWordGivenRule(rid2WordGivenRuleIndices(rule.rid)).prob(w),
						1.0 / G.W.asInstanceOf[Double] )
			} catch {
				//(case: new word)
				case (e:ArrayIndexOutOfBoundsException) => Double.NegativeInfinity
			}
		}
	}
	def ruleLogProb(rule:CkyRule):Double = {
		rule.rids.foldLeft(0.0){ (logScore:Double,rid:Int) => 
			val (headID,multID) = rid2RuleGivenHeadIndices(rid)
			logScore + U.safeLn(pRuleGivenHead(headID).prob(multID),0.01)
		}
	}

	def klex(sent:Sentence,y:(CkyRule,Int,Double)=>Boolean):Array[Int] = {
		(0 until sent.length).map{ (elem:Int) => 
			val word:Int = sent.words(elem)
			val pos:Int = sent.pos(elem)
			val num:Int = sent.nums(elem)
			//--Candidates
			val candidates:List[(CkyRule,Double)] = 
				//(proposals (nil + normal))
				(CKY_NIL.toList.filter{ _.head.nilWord.equals(U.w2str(word)) }
				
				::: 
				CKY_LEX.toList.filter{ !_.head.isNil }
						.filter{ (term:CkyRule) =>
					(  (term.child==Nonterminal('Word) 
								&& !U.isNum(word)) || //is word rule
					   (term.child==Nonterminal('Number) 
						 		&& U.isNum(word)) ) && //is number rule
					term.validInput( if(U.isNum(word)) num else word ) }  //is in range
				).map{ (term:CkyRule) => 
					//(a good lex rule)
					(term, lexLogProb(word,pos,term)) 
				}.toList
			//--Yield
			//(yield)
			var ok:Boolean = true
			candidates.sortWith{ 
					case ((rA:CkyRule,lPA:Double),(rB:CkyRule,lPB:Double)) =>
				if(lPA == lPB){
					if(rA == CKY_NIL){ true }
					else{ false }
				} else {
					lPA > lPB
				}
			}.foreach{ case (term,score) => 
				assert(term.isLex, "bad term returned in klex")
				if(ok && !y(term,elem,score)){ ok = false }
			}
			candidates.length
		}.toArray
	}
}

class CKYParser extends Parser with Serializable{
	import CKYParser._
	import Grammar._

	//-----
	// CKY
	//-----
	def cky[T](sent:Sentence,beam:Int):Array[ParseTree] = {
		assert(sent.length > 0, "Sentence of length 0 cannot be parsed")
		//--Create Chart
		val chart = makeChart(Thread.currentThread.getId)(sent.length,beam)
		assert(chart.length >= sent.length, "Chart is too small")
		//--Lex
		//(get probability function)
		val lexLogProb:(Sentence,(CkyRule,Int,Double)=>Boolean)=>Array[Int]
			= O.lexTagMethod match {
				case O.TagMethod.PCFG => CKYParser.klex(_,_)
				case O.TagMethod.GOLD =>
					Const.goldTag
			}
		//(fill chart)
		val lastScore:Array[Double] = (0 until sent.length).map{ 
			(i:Int) => Double.PositiveInfinity }.toArray
		lexLogProb(sent, (term:CkyRule,elem:Int,score:Double) => {
			//(add terms)
			assert(score <= 0.0, "Lex log probability of >0: " + score)
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
				Nonterminal.values.foreach{ head:Nonterminal => 
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
					assert(ruleLProb <= 0.0, "Log probability of >0: " + ruleLProb)
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
					assert(ruleLProb <= 0.0, "Log probability of >0: " + ruleLProb)
					assert(term.arity == 1, "Unary rule should be unary")
					val child:BestList = gram(chart,begin,end,term.child.id,BINARY)
					gram(chart,begin,end,term.head.id,UNARY).combine(term,child,
						(left:ChartElem,right:ChartElem) => { ruleLProb })
				}
				//(post-update tasks)
				if(O.kbestCKYAlgorithm < 3) {
					Nonterminal.values.foreach { head => 
						gram(chart,begin,end,head.id,BINARY).ensureEvaluated
						gram(chart,begin,end,head.id,UNARY).ensureEvaluated
					}
				}
			}
		}
		//--Return
		Array.concat(
			gram(chart,0,sent.length,Nonterminal('ROOT).id,UNARY).toArray,
			gram(chart,0,sent.length,Nonterminal('ROOT).id,BINARY).toArray
			).map{ x => x.deepclone }
	}

	//-----
	// Parse Method
	//-----

	override def beginIteration(iter:Int,feedback:Boolean,data:DataStore):Unit = {
		startTrack("Begin Iteration")
		//(clear last decoded)
		log("clearing last parses")
		if(feedback){
			guesses = List[GuessInfo]()
			corrects = List[GuessInfo]()
			timesToNormalize = List[()=>Any]()
		}
		endTrack("Begin Iteration")
	}

	override def endIteration(iter:Int,feedback:Boolean,data:DataStore):Unit = {
		if(feedback){
			startTrack("M Step")
			//(update rules)
			startTrack("rules")
			(0 until pRuleGivenHead.length).foreach{ (hid:Int) =>
				pRuleGivenHead(hid) = ruleESS(hid).runMStep
			}
			endTrack("rules")
			startTrack("lex")
			(0 until pWordGivenRule.length).foreach{ (id:Int) =>
				pWordGivenRule(id) = lexESS(id).runMStep
			}
			endTrack("lex")
			//(update times)
			startTrack("time")
			timesToNormalize.foreach{ (fn:()=>Any) => fn() }
			ranges.foreach{ case (r:Range,s:String) => r.runM }
			durations.foreach{ case (d:Duration,s:String) => d.runM }
			sequences.foreach{ case (d:Duration,s:String) => d.runM }
			endTrack("time")
			endTrack("M Step")
		}
		//--Debug
		forceTrack("Iteration Summary")
		reportInternal(false)
		endTrack("Iteration Summary")
	}


	private def reportInternal(writeParses:Boolean):Unit = {
		//--Debug Print
		//(best rules)
		forceTrack("rule scores (top by head)")
		Nonterminal.values.foreach{ (head:Nonterminal) =>
			log(FORCE,DIM, head + " -> " + 
				pRuleGivenHead(head.id).toString( new KeyPrinter[Int]{
					override def format(index:Int):String = {
						(0 until RULES.length).foreach{ (rid:Int) =>
							val (hid,multI) = rid2RuleGivenHeadIndices(rid)
							if(hid == head.id && multI == index){
								return RULES(rid).toString
							}
						}
						throw fail("Could not find rule corresponding to distribution term")
					}
				})
			)
		}
		endTrack("rule scores (top by head)")
		//(best lex)
		forceTrack("lex scores (top by rule)")
		(0 until RULES.length).foreach{ (rid:Int) =>
			val id = rid2WordGivenRuleIndices(rid)
			if(id >= 0){
				log(FORCE,DIM,RULES_STR(rid) + " -> " + 
					pWordGivenRule(id).toString( new KeyPrinter[Int]{
						override def format(w:Int):String = U.w2str(w)
					}))
			}
		}
		endTrack("lex scores (top by rule)")
		//(word reachability)
		forceTrack("impossible words")
		var unreachableWords:Int = 0
		(0 until RULES.length).foreach{ (rid:Int) =>
			val id = rid2WordGivenRuleIndices(rid)
			if(id >= 0){
				pWordGivenRule(id).zeroes.foreach{ (w:Int) =>
					log("unreachable: " + RULES_STR(rid) + " -> " + U.w2str(w))
					unreachableWords += 1
				}
			}
		}
		if(unreachableWords > 0){
			log(FORCE,RED,"WARNING: " + 
				unreachableWords + " rule->words with 0 probability")
		} else {
			log(FORCE,"no zero probability words")
		}
		endTrack("impossible words")
		//(write guesses)
		if(writeParses){
			startTrack("Creating Presentation")
			val b = new StringBuilder
			b.append(Const.START_PRESENTATION("Results"))
			guesses.sortBy( _.sid ).foldLeft(List[GuessInfo]()){ 
						case (acc:List[GuessInfo],term:GuessInfo) =>
							if(acc.length > 0 && acc(0).sid == term.sid) acc else term :: acc
					}.reverse.foreach{ (g:GuessInfo) =>
				val ref = if(g.feedback == null) null else g.feedback.ref
				if(g.isFailedParse){
					//(case: no parses for sentence)
					b.append(Const.AUTO_MISS(g.sid,g.sent,ref))
				} else {
					if(Parse(Nonterminal('ROOT),g.feedback.ref) != null){
						assert(g.feedback != null, "No feedback stored with " + g)
						assert(g.feedback.grounding != null, "No grounding stored with "+g)
						//(case: parsed sentence)
						b.append(Const.SLIDE(
								id=g.sid, correct=g.correct, tree=g.parse.asParseString(g.sent),
								probs=g.parse.asParseProbabilities(g.sent),
								guess=g.parseVal.ground(g.feedback.grounding).toString,
								gold=Parse(Nonterminal('ROOT),g.feedback.ref)
									.ground(g.feedback.grounding).toString,
								ground=g.feedback.grounding.toString,
								score=g.score
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
			endTrack("Creating Presentation")
		}
	}

	override def report = reportInternal(true)

	private def isEquivalentOutput(
			guess:Array[(Nonterminal,Temporal,Double)],
			gold:Array[(Nonterminal,Temporal,Double)]   ):(Boolean,String) = {
		if(guess.length != gold.length){ return (false,"different lengths") }
		//--Set Equality
		def setEquality(begin:Int,end:Int):(Boolean,String) = {
			for(i <- begin until end){
				if( !(begin until end).exists{ (j:Int) => 
							val (guessH,guessV,guessS) = guess(i)
							val (goldH,goldV,goldS) = gold(j)
							Parse(guessH,guessV,guessS).equals( Parse(goldH,goldV,goldS) )
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

	val parseLock = new ReentrantLock
	val updates = new java.io.FileWriter(Execution.touch("updates"))

	override def parse(i:Int, sent:Sentence, feedback:Boolean, identifier:String
			):(Array[Parse],Feedback=>Any)={
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
		val scored:Array[(Nonterminal,Temporal,Double)]
			= trees.map{ _.evaluate(sent) }
		val parses:Array[Parse] = scored.map{ case (tag,parse,s) => 
			Parse(tag,parse,s)
		}
		parses.zipWithIndex.foreach{ case (p:Parse,i:Int) =>
			p.tree = Some(trees(i).asParseString(sent))
			p.probs = Some(trees(i).asParseProbabilities(sent))
		}
		//(debug)
		val printBeam = if(O.printAllParses){ O.beam } else { 3 }
		val str = 
			"Guesses: " + 
				scored.slice(0,printBeam).map{ case (tag,parse,score) => 
					""+parse+"["+G.df.format(score)+"]"}.mkString(" or ")
		if(O.printAllParses) {
			log(FORCE,str)
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
				println(scoredReference mkString "\n")
				println("----ALGORITHM "+saveAlg+"----")
				println(scored mkString "\n")
			}
			assert( equivalent, "Inconsistent with algorithm 0: " + message)
			O.kbestCKYAlgorithm = saveAlg
		}

		parseLock.lock
		//--Return / Feedback
		//(add 'no result' guess)
		guesses = GuessInfo(identifier,null,Double.NaN,sent).wrong :: guesses
		//(return)
		val rtn = (	parses, 
			(feedback:Feedback) => {
				parseLock.lock
				log(if(feedback.isCorrect) "correct" 
				    else "missed ("+feedback.correct.length + " in beam)")
				//(best guess)
				val (head,parse,score) = scored(0)
				val guess = GuessInfo(identifier,trees(0),score,sent).feedback(feedback)
				guesses = {if(feedback.isCorrect) guess else guess.wrong} ::
					guesses.tail //note: tail to remove 'no result' guess
				//--Update Function
				def update(index:Int,offset:Int,count:Double) = {
					val (head,temporal,score) = scored(index)
					val parse = trees(index)
					startTrack("Correct: ["+offset+"] "
						+temporal+" ["+G.df.format(count)+"]")
					assert(!count.isNaN, "Trying to incorporate NaN count")
					assert(O.rulePrior.isZero || O.lexPrior.isZero || 
						count > Double.NegativeInfinity, "Parse has zero probability")
					//(count rules) NOTE: E-STEP HERE
					trees(index).traverse( 
							{ (rid:Int,nilTag:Option[Int]) =>
								//((update rule))
								val (hid,multI) = rid2RuleGivenHeadIndices(rid)
								ruleESS(hid).updateEStep(multI,count) 
							},
							{(rid:Int,i:Int) => 
								//((update rule))
								val (hid,multI) = rid2RuleGivenHeadIndices(rid)
								ruleESS(hid).updateEStep(multI,count) 
								//((update lex))
								try{
									updates.write(RULES_STR(rid)+"\t"+U.w2str(sent.words(i))+
										"\t"+count+"\n")
								} catch {
									case (e:Exception) => e.printStackTrace
								}
								val lexI = rid2WordGivenRuleIndices(rid)
								lexESS(lexI).updateEStep(sent.words(i),count)
							}
						)
					//(append guesses)
					corrects = GuessInfo(identifier,parse,count,sent) :: corrects
					//(update time)
					val ground = Range(feedback.grounding,feedback.grounding)
					timesToNormalize = {() => { temporal.traverse(ground,offset,
							(term:Temporal,trueOffset:Long)=>{
								term.runM
							})
						}} :: timesToNormalize
					forceTrack("Time E " + temporal)
					temporal.traverse(ground,offset,
						(term:Temporal,trueOffset:Long) => {
//							log(FORCE,""+term+" is offset by " + trueOffset)
							assert(O.timeDistribution != O.Distribution.Point || offset == 0L,
								"nonzero offset with point distribution")
							term.updateE(ground,trueOffset,
								if(O.hardEM) 0.0 else U.safeLn(count) 
							)
						})
					endTrack("Time E " + temporal)
					//(end)
					endTrack("Correct: ["+offset+"] "
						+temporal+" ["+G.df.format(count)+"]")
				}
				//--Run Update
				forceTrack("Update")
				//(get correct)
				//  index,offset
				val correct:Array[(Int,Int)] = O.ckyCountType match {
					case O.CkyCountType.all =>
						feedback.correct.map{ case (i:Int,o:Int,score:Double) => (i,o) }
					case O.CkyCountType.bestAll => feedback.tiedBest
					case O.CkyCountType.bestRandom => Array[(Int,Int)](
						(feedback.bestIndex,feedback.bestOffset))
					case O.CkyCountType.bestShallow => {
						//((get shallowest tree))
						val ((bestI,bestOffset),depth) =
							feedback.tiedBest.foldLeft(((-1,-1),Int.MaxValue)){
								case ((argmin,min),(index,offset)) =>
									val candMin = trees(index).maxDepth
									if(candMin < min) ((index,offset),candMin) else (argmin,min)
								}
						assert(!feedback.hasCorrect || bestI >= 0,"Bad shallow computation")
						//((create list))
						if(bestI >= 0){ 
							Array[(Int,Int)]((bestI,bestOffset))
						} else {
							Array[(Int,Int)]()
						}
					}
					case O.CkyCountType.shortWithOffsetZero => {
						//((has an offset zero term?))
						val hasZeroOffset:Boolean = feedback.correct.exists{ _._2 == 0 }
						//((get shortest length))
						val shortest:Int = feedback.correct.foldLeft(Int.MaxValue){
								case (shortest:Int,(index:Int,offset:Int,score:Double)) =>
							if(hasZeroOffset && offset == 0){
								val parse = trees(index)
								math.min(shortest,parse.trim(sent).length)
							} else {
								shortest
							}
						}
						//((get matching trees))
						val matching:Array[(Int,Int,Double)] = feedback.correct.filter{
								case (index:Int,offset:Int,score:Double) =>
							trees(index).trim(sent).length <= shortest &&
								(!hasZeroOffset || offset == 0)
						}
						//((create list))
						matching.map{ case (i:Int,o:Int,score:Double) => (i,o) }
					}
					case _ => throw fail("Unknown case: " + O.ckyCountType)
				}
				//  index,offset,raw_count
				val correctWithRaw:Array[(Int,Int,Double)] = correct.map{ 
						case (index:Int,offset:Int) =>
					//((get raw count))
					val rawCount:Double = 
						if(O.hardEM) 1.0
						else math.exp(scored(index)._3) *
						     scored(index)._2.prob(
								 	Range(feedback.grounding,feedback.grounding),offset)
					assert(rawCount <= 1.0 && rawCount >= 0.0 && !rawCount.isNaN, 
						"invalid raw count: " + rawCount)
					(index,offset,rawCount)
				}
				//(get total prob)
				val totalProb:Double = correctWithRaw.foldLeft(0.0){
						case (soFar:Double,(index:Int,offset:Int,raw:Double)) =>
					soFar + raw
				}
				//(normalize score)
				val correctCount:Double = correct.length.asInstanceOf[Double]
				//  index,offset,count
				val correctWithProbs:Array[(Int,Int,Double)] = correctWithRaw.map{
						case (index:Int,offset:Int,raw:Double) =>
					val count:Double = O.ckyCountNormalization match {
						case O.CkyCountNormalization.none => raw
						case O.CkyCountNormalization.uniform => 1.0 / correctCount
						case O.CkyCountNormalization.proportional => raw / correctCount
						case O.CkyCountNormalization.distribution => {
							assert(!totalProb.isNaN, "Total Probability is NaN")
							if(totalProb == 0.0){
								1.0 / correctCount
							} else {
								raw / totalProb
							}
						}
					}
					(index,offset,count)
				}
				//(update)
				correctWithProbs.foreach{ case (index:Int,offset:Int,count:Double) =>
					update(index,offset,count) 
				}
				parseLock.unlock
				endTrack("Update")
			}
		)
		//--Return
		parseLock.unlock
		rtn
	}
}
