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
import org.goobs.util.Indexer
import org.goobs.nlp._

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
// AUXILLIARY
//------------------------------------------------------------------------------
case class TimeSent(id:Int,words:Array[Int],pos:Array[Int],nums:Array[Int]) 
		extends Sentence{
	//<<required overrides>>
	override def apply(i:Int):Int = words(i)
	override def length:Int = words.length
	override def gloss(i:Int):String = {
		if(U.isNum(words(i))){
			assert(nums(i) != Int.MinValue, 
				"Returning number that was not set: " + U.w2str(words(i)))
			nums(i).toString
		} else {
			U.w2str(words(i))
		}
	}
	//<<optional overrides>>
	override def asNumber(i:Int):Int = nums(i)
	override def asDouble(i:Int):Double = nums(i).toDouble
	//<<object overrides>>
	override def toString:String = words.map{ U.w2str(_) }.mkString(" ")
}

//------------------------------------------------------------------------------
// GRAMMAR
//------------------------------------------------------------------------------

class TimeUnary(lambda:Option[Any=>Any],_parent:NodeType,_child:NodeType)
		extends CKYUnary(lambda,_parent,_child) {
	def this(lambda:Any=>Any,_parent:NodeType,_child:NodeType) 
		= this(Some(lambda),_parent,_child)
	def this(_parent:NodeType,_child:NodeType) = this(None,_parent,_child)
	override def equals(o:Any) = o match {
		case (a:AnyRef) => this eq a
		case _ => false
	}
}
class TimeBinary(lambda:Option[(Any,Any)=>Any],_parent:NodeType,
		_leftChild:NodeType,_rightChild:NodeType) 
		extends CKYBinary(lambda,_parent,_leftChild,_rightChild) {
	def this(lambda:(Any,Any)=>Any,_parent:NodeType,
			_leftChild:NodeType,_rightChild:NodeType)
		= this(Some(lambda),_parent,_leftChild,_rightChild)
	def this(_parent:NodeType,_leftChild:NodeType,_rightChild:NodeType)
		= this(None,_parent,_leftChild,_rightChild)
	override def equals(o:Any) = o match {
		case (a:AnyRef) => this eq a
		case _ => false
	}
}

object Grammar {
	case class NIL()
	//----------
	// NODE TYPES
	//----------
	//--Init Function
	def init(indexer:Indexer[String]) = {
		//(lex)
		NodeType.makePreterminal('Word)
		NodeType.makePreterminal('Number)
		//(basic types)
		NodeType.make('Range)
		NodeType.make('Duration)
		NodeType.make('Sequence)
		NodeType.make('NUM)
		//(some functions)
		NodeType.make("F_{N}2S")
		//(nils)
		if(O.lexNils){
			indexer.foreach{ (word:String) =>
				NodeType.make(Symbol("NIL-"+word), 'nil)
			}
		} else {
			NodeType.make('NIL, 'nil)
		}
	}
	//--Other NodeTypes
	import scala.collection.immutable.Set
	val rangeTypes = List("R","S")
	val durationTypes = List("D","S")
	//(arity-2 functions)
	//((like rd2r))
	lazy val fn2 = {rangeTypes.foldLeft(List[(NodeType,Symbol,Symbol)]()){
			case (soFar:List[(NodeType,Symbol,Symbol)],r:String) =>
		durationTypes.foldLeft(List[(NodeType,Symbol,Symbol)]()){
				case (soFar:List[(NodeType,Symbol,Symbol)],d:String) =>
			( NodeType.make(Symbol("F_{"+r+d+"}2"+r)),Symbol(r),Symbol(d)
				) :: soFar
		} ::: soFar
	}.toSet ++
	//((like rr2r))
	rangeTypes.foldLeft(List[(NodeType,Symbol,Symbol)]()){
			case (soFar:List[(NodeType,Symbol,Symbol)],r1:String) =>
		rangeTypes.foldLeft(List[(NodeType,Symbol,Symbol)]()){
				case (soFar:List[(NodeType,Symbol,Symbol)],r2:String) =>
			(NodeType.make(Symbol("F_{"+r1+r2+"}2"+r1)),
				Symbol(r1),Symbol(r2)) :: soFar
		} ::: soFar
	}}.toList
	//(arity-1 functions)
	//((like r2r))
	lazy val fn1 = {rangeTypes.foldLeft(List[((NodeType,Symbol),Symbol)]()){
			case (soFar:List[((NodeType,Symbol),Symbol)],r:String) =>
		((NodeType.make(Symbol("F_{"+r+"}2"+r)),Symbol(r)),Symbol(r)) :: soFar
	}.toSet ++
	//((n2r))
	List[((NodeType,Symbol),Symbol)](
		((NodeType(Symbol("F_{N}2S")),'S), 'N)
	) ++
	//((like d2d))
	durationTypes.foldLeft(List[((NodeType,Symbol),Symbol)]()){
			case (soFar:List[((NodeType,Symbol),Symbol)],d:String) =>
		((NodeType.make(Symbol("F_{"+d+"}2"+d)),Symbol(d)),Symbol(d)) :: soFar
	}}.toList
	//--From Short Version
	def fromShort(short:String):NodeType = fromShort(Symbol(short))
	def fromShort(short:Symbol):NodeType = {
		short match {
			case 'R => NodeType('Range)
			case 'S => NodeType('Sequence)
			case 'D => NodeType('Duration)
			case 'N => NodeType('Number)
			case _ => throw fail("No such short form: " + short)
		}
	}

	//----------
	// TIME TERMS
	//----------
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
	var durations = 
		{if(O.useTime) List[(Duration,String)]((ASEC,"Sec:D"),(AMIN,"Min:D"),
			(AHOUR,"Hour:D")) else List[(Duration,String)]()} :::
		List[(Duration,String)](
			(ADAY,"Day:D"),(AWEEK,"Week:D"),(AMONTH,"Month:D"),(AQUARTER,"Quarter:D"),
			(AHALFYEAR,"HalfYear:D"),
			(AYEAR,"Year:D"),(ADECADE,"Decade:D"),(ACENTURY,"Century:D")
			)
	durations = {if(!O.functionalApproximate){
			durations ::: durations.map{ case (d:Duration,name:String) =>
				(~d,"~"+name)
			}
		} else {
			durations
		}}
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
		{if(O.useTime && !O.ignoreTimeSequences) List[(Sequence,String)](
			(SEC,"Sec:S"),(MIN,"Min:S"),
			(HOUR,"Hour:S")) else List[(Sequence,String)]()} :::
		List[(Sequence,String)](
			(DAY,"Day:S"),(WEEK,"Week:S"),(MONTH,"Month:S"),(QUARTER,"Quarter:S"),
			(HALFYEAR,"HalfYear:S"),(YEAR,"Year:S")
			) :::
		Nil
	val indexedSequences:List[(Array[Sequence],String)]
		= List[(Array[Sequence],String)](
			(DOW,"DOW$(n^{th})$"),
			(WOM,"WOM$(n^{th})$"),
			(WOY,"WOY$(n^{th})$"),
			(MOY,"MOY$(n^{th})$"),
			(QOY,"QOY$(n^{th})$")
		) :::
		(1 to 7).map{(i:Int) => (DOWOM(i), DOW_STR(i-1)+"$(n)$")}.toList :::
		Nil
	
	//----------
	// RULES
	//----------
	private lazy val NAMED_RULES:Array[(GrammarRule,String)] = {
		def hack[A,Z](fn:A=>Z):Any=>Any = fn.asInstanceOf[Any=>Any]
		def hack2[A,B,Z](fn:(A,B)=>Z):(Any,Any)=>Any 
			= fn.asInstanceOf[(Any,Any)=>Any]
		var rtn = List[(GrammarRule,String)]()
		//--Lex
		rtn = rtn ::: ranges.map{ case (r:Range,s:String) => 
			(new TimeUnary(hack((w:Int) => r), NodeType('Range), 
				NodeType('Word)), s)}
		rtn = rtn ::: durations.map{ case (d:Duration,s:String) => 
			(new TimeUnary(hack((w:Int) => d), NodeType('Duration), 
				NodeType('Word)), s)}
		rtn = rtn ::: sequences.map{ case (d:Duration,s:String) => 
			(new TimeUnary(hack((w:Int) => d), NodeType('Sequence), 
				NodeType('Word)), s)}
		//(nil)
		rtn = rtn ::: 
			{if(O.lexNils){
				assert(G.wordIndexer.size > 0, "Haven't initialized indexer yet")
				G.wordIndexer.map{ (word:String) =>
					(new TimeUnary(hack((w:Int) => new NIL),
						NodeType(Symbol("NIL-"+word)), NodeType('Word)), "nil-"+word)
				}.toList
			} else {
				List[(GrammarRule,String)]((new TimeUnary(hack((w:Int) => new NIL),
					NodeType('NIL), NodeType('Word)), "nil"))
			}}
		//(numbers)
		rtn = rtn ::: List[(GrammarRule,String)](
			(new TimeUnary(hack((num:Int) =>  num ),
				NodeType('NUM), NodeType('Number)),
				"NUM"),
			(new TimeUnary(hack((num:Int) =>  MOH(num) ),
				NodeType('Sequence), NodeType('Number)) 
				.restrict( (w:Int) => w >= 0 && w < 60 ),
				"moh(n):S"),
			(new TimeUnary(hack((num:Int) =>  HOD(num) ),
				NodeType('Sequence), NodeType('Number))
				.restrict( (w:Int) => w >= 0 && w < 24 ),
				"hod(n):S"),
			(new TimeUnary(hack((num:Int) =>  DOM(num) ),
				NodeType('Sequence), NodeType('Number))
				.restrict( (w:Int) => w >= 1 && w <= 31 ),
				"dom(n):S"),
			(new TimeUnary(hack((num:Int) =>  MOY(num) ),
				NodeType('Sequence), NodeType('Number))
				.restrict( (w:Int) => w >= 1 && w <= 12 ),
				"moy(n):S"),
			(new TimeUnary(hack((num:Int) =>  YOC(num) ),
				NodeType('Sequence), NodeType('Number)) 
				.restrict( (w:Int) => w >= 0 && w < 100 ),
				"yoc(n):S"),
			(new TimeUnary(hack((num:Int) =>  DOC(num) ),
				NodeType('Sequence), NodeType('Number)) 
				.restrict( (w:Int) => w >= 0 && w < 10 ),
				"doc(n):S"),
			(new TimeUnary(hack((num:Int) =>  YOD(num) ),
				NodeType('Sequence), NodeType('Number)) 
				.restrict( (w:Int) => w >= 0 && w < 10 ),
				"yod(n):S"),
			(new TimeUnary(hack((num:Int) =>  THEYEAR(num) ),
				NodeType('Range), NodeType('Number)),
				"year(n):R"),
			(new TimeUnary(hack((num:Int) =>  CENTURY(num)),
				NodeType('Range), NodeType('Number))
				.restrict( (w:Int) => w > -100 && w < 100 ),
				"century(n):R")
			)
		//(indices)
		rtn = rtn ::: indexedSequences.map{ case (fn:Array[Sequence],name:String) =>
			(new TimeUnary(hack((w:Int) =>  
					(n:Int) => {
						if(n < 0 || n >= fn.length){
							new NoTime
						} else {
							fn(n)
						}
					}
				), NodeType("F_{N}2S"), NodeType('Word) ),
				name
			)
		}
		
		//--Arity 1 Functions
		//(util)
		case class F1Info[A](
			fn:(_<:A)=>_<:A,name:String,validA:List[Symbol])
		//(define)
		val function1 = List[F1Info[Temporal]](
			F1Info(move(_:Sequence,-1L),"moveLeft1",List('S)),   //last [sequence]
			F1Info(move(_:Sequence,1L),"moveRight1",List('S)),   //next [sequence]
			F1Info(fuzzify,"fuzzify",List('D))                   //around
		) ::: {if(O.functionalUnboundedRange){
				List[F1Info[Temporal]](
					F1Info(toPast,"toPast",List('R,'S)),             //recent months
					F1Info(toFuture,"toFuture",List('R,'S))          //future months
				)
			} else {
				Nil
			}
		}
		//(apply)
		rtn = rtn ::: {
			fn1.foldLeft(List[(GrammarRule,String)]()){
					case (soFar:List[(GrammarRule,String)],
						((fnNode:NodeType,head:Symbol),a:Symbol)) =>
				//(consume A on the left)
				{(new TimeBinary(
					hack2((a:Any,fn:(Any)=>Temporal) => fn(a)),       //function
					fromShort(head),                                  //head
					fromShort(a),                                     //left
					fnNode ),                                         //right
				"$x:"+a.name+"$") ::                                //name
				//(consume A on the right)
				(new TimeBinary(
					hack2((fn:(Any)=>Temporal,a:Any) => fn(a)),       //function
					fromShort(head),                                  //head
					fnNode,                                           //left
					fromShort(a) ),                                   //right
				"$x:"+a.name+"$") ::                                //name
				Nil} ::: soFar
			}
		}
		//(intro)
		function1.foreach{ (info:F1Info[Temporal]) =>
			//(for every argA...)
			rtn = rtn ::: info.validA.foldLeft(List[(GrammarRule,String)]()){ 
					case (soFarOuter:List[(GrammarRule,String)],a:Symbol) =>
				//(create rule)
				val rule = (new TimeUnary(
					hack((w:Int) => info.fn),
					NodeType("F_{"+a.name+"}2"+a.name),
					NodeType('Word)),
					info.name+"$(-:"+a.name+"):"+a.name+"$" )
				//(append rule)
				rule :: soFarOuter
			}
		}

		//--Arity 2 Functions
		//(util)
		case class F2Info[A,B](
			fn:(_<:A,_<:B)=>_<:A,name:String,validA:List[Symbol],validB:List[Symbol])
		//(define)
		val function2 = List[F2Info[Temporal,Temporal]](
			F2Info(
				if(O.cannonicalShifts){ cannonicalLeft } else shiftLeft,
				"shiftLeft",List('R,'S),List('D)),                       //last/ago
			F2Info(
				if(O.cannonicalShifts){ cannonicalRight } else shiftRight,
				"shiftRight",List('R,'S),List('D)),                      //next
			F2Info(shrinkBegin,"shrinkBegin",List('R,'S),List('D)),    //first
			F2Info(shrinkBegin,"shrinkEnd",List('R,'S),List('D)),      //last
			F2Info(catLeft,"catLeft",List('R),List('D)),               //past
			F2Info(catRight,"catRight",List('R),List('D))              //coming
//			F2Info(cons,"cons",List('R,'S),List('R,'S))                //from...until
		)
		//(apply)
		rtn = rtn ::: {
			fn2.foldLeft(List[(GrammarRule,String)]()){
					case (soFar:List[(GrammarRule,String)],
					      (fn:NodeType,a:Symbol,b:Symbol)) =>
				//(consume B on the left)
				{(new TimeBinary(
					hack2((b:Temporal,fn:(Temporal,Temporal)=>Temporal) =>     //function
						fn(_:Temporal,b)),
					NodeType(Symbol("F_{"+a.name+"}2"+a.name)),                //head
					fromShort(b),                                              //left
					fn),                                                       //right
				"$f(-:"+a.name+",x:"+b.name+"):"+a.name+"$") ::              //name
				//(consume B on the right)
				(new TimeBinary(
					hack2((fn:(Temporal,Temporal)=>Temporal,b:Temporal) =>     //function
						fn(_:Temporal,b)), 
					NodeType(Symbol("F_{"+a.name+"}2"+a.name)),                //head
					fn,                                                        //left
					fromShort(b)),                                             //right
				"$f(-:"+a.name+",x:"+b.name+"):"+a.name+"$") ::              //name
				Nil} ::: soFar
			}
		}
		//(ref augmented apply)
		rtn = rtn ::: fn2.filter {
				case (fn:NodeType,a:Symbol,b:Symbol) => 
					a == 'R || b == 'R
			}.foldLeft(List[(GrammarRule,String)]()){
				case (soFar:List[(GrammarRule,String)],
				     (fn:NodeType,a:Symbol,b:Symbol))=>
			{if(b == 'R){
				//(consume A on the left -- B is Range)
				(new TimeBinary(
					hack2((a:Temporal,fn:(Temporal,Range)=>Temporal) => {      //function
						fn(a,REF)}),
					fromShort(a),                                     //head
					fromShort(a),                                     //left
					fn ),                                                      //right
				"$r:"+a.name+"$") ::                                         //name
				//(consume A on the right -- B is Range)
				(new TimeBinary(
					hack2((fn:(Temporal,Range)=>Temporal,a:Temporal) => {      //function
						fn(a,REF)}),
					fromShort(a),                                     //head
					fn,                                                        //left
					fromShort(a) ),                                   //right
				"$r:"+a.name+"$") ::                                         //name
				Nil
			} else {List[(GrammarRule,String)]()} :::
			{if(a == 'R){
				//(consume B on the left -- A is Range)
				(new TimeBinary(
					hack2((b:Temporal,fn:(Range,Temporal)=>Temporal) => {      //function
						fn(REF,b)}),
					fromShort(a),                                     //head
					fromShort(b),                                     //left
					fn ),                                                      //right
				"$r:"+a.name+"$") ::                                         //name
				//(consume B on the right -- A is Range)
				(new TimeBinary(
					hack2((fn:(Range,Temporal)=>Temporal,b:Temporal) => {      //function
						fn(REF,b)}),
					fromShort(a),                                     //head
					fn,                                                        //left
					fromShort(b) ),                                   //right
				"$r:"+a.name+"$") ::                                         //name
				Nil
			} else { Nil } }} ::: soFar
		}
		//(intro)
		function2.foreach{ (info:F2Info[Temporal,Temporal]) =>
			//(for every argA...)
			rtn = rtn ::: info.validA.foldLeft(List[(GrammarRule,String)]()){ 
					case (soFarOuter:List[(GrammarRule,String)],a:Symbol) =>
				//(for every argB...)
				soFarOuter ::: info.validB.foldLeft(List[(GrammarRule,String)]()){
						case (soFarInner:List[(GrammarRule,String)],b:Symbol) =>
					//(create rule)
					val rule = (new TimeUnary(
						hack((w:Int) => info.fn),
						NodeType("F_{"+a.name+b.name+"}2"+a.name),
						NodeType('Word)),
						info.name+"$(-:"+a.name+",-:"+b.name+"):"+a.name+"$" )
					//(append rule)
					rule :: soFarInner
				}
			}
		}
		
		//--Multiply Duration
		rtn = rtn ::: List[(GrammarRule,String)](
			(new TimeBinary(
				hack2( (d:Duration,n:Int) => d*n ),
				NodeType('Duration),
				NodeType('Duration), 
				NodeType('NUM)
				), "D*n"),
			(new TimeBinary(
				hack2( (n:Int,d:Duration) => d*n ),
				NodeType('Duration), 
				NodeType('NUM), 
				NodeType('Duration)
				), "n*D")
			)

		//--Intersect
		rtn = rtn ::: rangeTypes.foldLeft(List[(GrammarRule,String)]()) {
				case (soFar:List[(GrammarRule,String)],rA:String) =>
			rangeTypes.foldLeft(List[(GrammarRule,String)]()){
					case (soFarInner:List[(GrammarRule,String)],rB:String) =>
				val head = {if(rA == "R" || rB == "R") "R" else rA}
				(new TimeBinary(
					hack2( (r1:Range,r2:Range) => r1 ^ r2), 
					fromShort(head), 
					fromShort(rA),
					fromShort(rB) ),
				"$a + b:"+head+"$") :: soFarInner
			} ::: soFar
		}

		//--NIL Identities
		rtn = rtn ::: NodeType.all.filter{ (x:NodeType) =>
					!x.flag('nil) && x != NodeType.ROOT && !x.isPreterminal}.
				foldLeft(List[(GrammarRule,String)]()){
				case (soFar:List[(GrammarRule,String)],term:NodeType) => 
			//(get possible nils)
			val nilList:List[NodeType] =
				{if(O.lexNils){
					G.wordIndexer.map{ (word:String) => NodeType(Symbol("NIL-"+word)) }
				} else {
					List[NodeType](NodeType('NIL))
				}}.toList
			//(add identities)
			nilList.foldLeft(List[(GrammarRule,String)]()){ 
					case (lst:List[(GrammarRule,String)],nil:NodeType) =>
				(new TimeBinary(
						hack2( (x:Any,n:NIL) => x ),
						term,
						term,
						nil
					),"$x:"+term.name+"$") ::
				(new TimeBinary(
					hack2( (n:NIL,x:Any) => x),
					term,
					nil,
					term
					),"$x:"+term.name+"$") :: lst
			} ::: soFar
		}

		//--Return
		rtn.foreach{ case (r:GrammarRule,s:String) => r.setGloss(s) }
		rtn.toArray
	}
	
	lazy val RULES:Array[GrammarRule] = {
		def hack[A,Z](fn:A=>Z):Any=>Any = fn.asInstanceOf[Any=>Any]
		def hack2[A,B,Z](fn:(A,B)=>Z):(Any,Any)=>Any 
			= fn.asInstanceOf[(Any,Any)=>Any]
		var rtn = List[GrammarRule]()
		
		//--Named Rules
		rtn = rtn ::: NAMED_RULES.map( _._1 ).toList

		//--ROOT
		rtn = rtn ::: List[GrammarRule](
			//(rules)
			new TimeUnary(hack((r:Range) => r),
				NodeType.ROOT, NodeType('Range)), 
			new TimeUnary(hack((d:Duration) => d),
				NodeType.ROOT, NodeType('Duration)), 
			new TimeUnary(hack((s:Range) => s), //note: range
				NodeType.ROOT, NodeType('Sequence))
			) ::: { if(O.allowPartialTime)
								List[TimeUnary]( 
									new TimeUnary(hack((fn:Range=>Range) => fn),
										NodeType.ROOT, NodeType('F_R2R)) )
							else List[TimeUnary]() }
		//--LEX
		rtn = rtn ::: List[GrammarRule](
			GrammarRule(
				(sent:Option[Sentence],index:Int) => sent.get.apply(index),
				NodeType('Word)),
			GrammarRule(
				(sent:Option[Sentence],index:Int) => sent.get.asNumber(index), 
				NodeType('Number)
				).restrict( U.isInt(_) )
			)
		//--Return
		rtn.toArray
	}
	
	lazy val RULES_STR:Array[String] = {
		val rtn = RULES.map{ _.parent.toString }
		for(i <- 0 until NAMED_RULES.length){
			assert(RULES(i) == NAMED_RULES(i)._1, "name mismatch")
			rtn(i) = NAMED_RULES(i)._2
		}
		rtn
	}
	
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
					if(guess.begin == Time.DAWN_OF && gold.begin == Time.DAWN_OF){
						(Duration.ZERO,Duration.ZERO) //case: past
					} else if(guess.end == Time.END_OF && gold.end == Time.END_OF){
						(Duration.ZERO,Duration.ZERO) //case: future
					} else if(guess.begin == Time.DAWN_OF && gold.begin != Time.DAWN_OF){
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
			//(get diff)
			val d = diff(gold,guess,false)
			//(check timex consistency)
			if(U.sumDiff(d) > O.exactMatchThreshold){
				import edu.stanford.nlp.time.JodaTimeUtils._
				val (tGold, tGuess) = (gold,guess) match {
					case (a:UnkTime, b:Temporal) => {("not", "equal")}
					case (a:Temporal, b:NoTime) => {("not", "equal")}
					case (a:GroundedRange,b:GroundedRange) => {
						(timexDateValue(a.begin.base, a.end.base),
							timexDateValue(b.begin.base,b.end.base))
					}
					case (a:FuzzyDuration,b:FuzzyDuration) => {
						(timexDurationValue(a.interval.base,true),
							timexDurationValue(b.interval.base,true))
					}
					case (a:GroundedDuration,b:GroundedDuration) => {
						(timexDurationValue(a.interval.base),
								timexDurationValue(b.interval.base))
					}
					case _ => ("not","equal")
				}
				if(tGold.equals(tGuess)){
					err("Timexes match but " +
						"difference is nonzero: gold="+tGold+" guess="+tGuess+
						"  myGuess="+guess+"  inferredGold="+gold+" (diff="+d+") :: "+ 
						tree.orNull)
				}
			}
			//(debug)
			assert(O.timeDistribution != O.Distribution.Point ||
				offset == 0L ||
				prob == 0.0,
				"Time returned distribution when it shouldn't have: " 
					+ guess + " (offset=" + offset + ") [prob=" + prob + "]")
			//(return)
			(d,prob,offset.toInt)
		}
	}
	
	def ground(ground:Time):Temporal
		= if(value.exists(Range(ground,ground),0)){
				value(Range(ground,ground))
			} else { new NoTime }
}
