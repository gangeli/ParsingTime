package time

//(java)
import java.util.{List => JList}
import java.lang.{Integer => JInt}
import java.util.Calendar
//(scala)
import scala.collection.JavaConversions._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.immutable.Map
//(jodatime)
import org.joda.time.DateTimeZone
import org.joda.time.DateTime
//(lib)
import org.goobs.exec.Execution
import org.goobs.stanford.JavaNLP._
import org.goobs.stats._
import org.goobs.util.Indexer
import org.goobs.nlp._
import org.goobs.stanford.SerializedCoreMapDataset
import org.goobs.util.Stopwatch
import org.goobs.util.Static._
//(stanford)
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.pipeline.Annotator
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.sequences.SeqClassifierFlags
import edu.stanford.nlp.sequences.FeatureFactory
import edu.stanford.nlp.time.JodaTimeUtils
import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.time.TimeAnnotations._
import edu.stanford.nlp.time.{Timex => StanfordTimex}

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
//	override def hashCode:Int = System.identityHashCode(this)
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
//	override def hashCode:Int = System.identityHashCode(this)
}
class TimeLex(lambda:((Option[Sentence],Int)=>Any), parent:NodeType) 
		extends CKYLex(lambda,parent) {
	override def equals(o:Any) = o match {
		case (a:AnyRef) => this eq a
		case _ => false
	}
//	override def hashCode:Int = System.identityHashCode(this)
}

@SerialVersionUID(1L)
class Grammar(index:Indexing,lex:Lex,NodeType:NodeTypeFactory) extends Serializable {
	import lex._
	case class NIL()
	//----------
	// NODE TYPES
	//----------
	def getNumFromOrder(order:Int,typ:NumberType.Value):Symbol = {
		Symbol("NUM*$10^"+order+"$:"+typ.toString)
	}
	def getNum(num:Int,typ:NumberType.Value):Symbol = {
		getNumFromOrder(num.toString.length-1,typ)
	}
	//--Init Function
	val nums:Seq[NodeType] = {
		(0 to 10).map{ (orderOfMagnitude:Int) =>
			NumberType.values.map{ (numberType:NumberType.Value) =>
				NodeType.makePreterminal(
					getNumFromOrder(orderOfMagnitude,numberType),
					'num, Symbol(orderOfMagnitude.toString), Symbol(numberType.toString))
			}
		}.flatten.toList
	}
	var nils:Seq[NodeType] = List[NodeType]()
	def mkNils(index:Indexing, num:Int) {
		nils = if(O.lexNils){
			(0 until index.W).map{ (w:Int) =>
				if(w == index.NUM) {
					nums.map{ (num:NodeType) =>
						NodeType.make(Symbol("NIL-"+num.toString), 'nil, 'nilnum)
					}
				} else {
					val word:String = index.w2str(w)
					List[NodeType](NodeType.makePreterminal(Symbol("NIL-"+word), 'nil))
				}
			}.flatten
		} else {
			List[NodeType](NodeType.makePreterminal('NIL, 'nil))
		}
	}
	
	//--Added Terms
	//(basic types)
	NodeType.make('Range)
	NodeType.make('Duration)
	NodeType.make('Sequence)
	//(sequences)
	NodeType.make("moh(n)")
	NodeType.make("hod(n)")
	NodeType.make("dom(n)")
	NodeType.make("moy(n)")
	NodeType.make("yoc(n)")
	NodeType.make("doc(n)")
	NodeType.make("yod(n)")
	NodeType.make("year(n)")
	NodeType.make("century(n)")
	//(nils)
	mkNils(index,index.NUM)
	//--Numbers
	assert(!nums.isEmpty, "no number terms!")
	
	//--Other NodeTypes
	import scala.collection.immutable.Set
	val rangeTypes = List("R","S")
	val durationTypes = List("D","S")
	//(arity-2 functions)
	//((like rd2r))
	val fn2 = {rangeTypes.foldLeft(List[(NodeType,Symbol,Symbol)]()){
			case (soFar:List[(NodeType,Symbol,Symbol)],r:String) =>
		durationTypes.foldLeft(List[(NodeType,Symbol,Symbol)]()){
				case (soFar:List[(NodeType,Symbol,Symbol)],d:String) =>
			( NodeType.make(Symbol("F_{"+r+d+"}2"+r),'fn),Symbol(r),Symbol(d)
				) :: soFar
		} ::: soFar
	}.toSet ++
	//((like rr2r))
	rangeTypes.foldLeft(List[(NodeType,Symbol,Symbol)]()){
			case (soFar:List[(NodeType,Symbol,Symbol)],r1:String) =>
		rangeTypes.foldLeft(List[(NodeType,Symbol,Symbol)]()){
				case (soFar:List[(NodeType,Symbol,Symbol)],r2:String) =>
			(NodeType.make(Symbol("F_{"+r1+r2+"}2"+r1),'fn),
				Symbol(r1),Symbol(r2)) :: soFar
		} ::: soFar
	}}.toList
	//(arity-1 functions)
	//((like r2r))
	val fn1 = {rangeTypes.foldLeft(List[((NodeType,Symbol),Symbol)]()){
			case (soFar:List[((NodeType,Symbol),Symbol)],r:String) =>
		((NodeType.make(Symbol("F_{"+r+"}2"+r),'fn),Symbol(r)),Symbol(r)) :: soFar
	}.toSet ++
	//((n2r))
	List[((NodeType,Symbol),Symbol)](
		((NodeType.make(Symbol("F_{N0th}2S"),'fn),'S), 'N0th)
	) ++
	//((like d2d))
	durationTypes.foldLeft(List[((NodeType,Symbol),Symbol)]()){
			case (soFar:List[((NodeType,Symbol),Symbol)],d:String) =>
		((NodeType.make(Symbol("F_{"+d+"}2"+d),'fn),Symbol(d)),Symbol(d)) :: soFar
	}}.toList
	
	//--Preterminal Equivalents
	//(add lex entries)
	NodeType.all.filter{ _.flag('fn) }.foreach{ (nt:NodeType) =>
		if(!nt.isPreterminal && !nt.flag('nil)){
			NodeType.makePreterminal(nt.toString + "_")
		}
	}
	
	//--From Short Version
	def fromShort(short:String):NodeType = fromShort(Symbol(short))
	def fromShort(short:Symbol):NodeType = {
		short match {
			case 'R => NodeType('Range)
			case 'S => NodeType('Sequence)
			case 'D => NodeType('Duration)
			case 'N0th => NodeType(getNumFromOrder(0,NumberType.ORDINAL))
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
			.name(DOW_STR(i-1)),DOW_STR(i-1)) ).toList :::
		(1 to 12).map(i=>(MOY(i).asInstanceOf[RepeatedRange]
			.name(MOY_STR(i-1)),MOY_STR(i-1)) ).toList :::
//		(1 to 4).map(i=>(QOY(i).asInstanceOf[RepeatedRange]
//			.dense.name(QOY_STR(i-1)),QOY_STR(i-1)) ).toList ::: 
		(1 to 4).map(i=>(SEASON(i).asInstanceOf[RepeatedRange]
			.name(SEASON_STR(i-1)),SEASON_STR(i-1)) ).toList ::: 
		(1 to 4).map(i=>(TOD(i).asInstanceOf[RepeatedRange]
			.name(TOD_STR(i-1)),TOD_STR(i-1)) ).toList ::: 
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
	//(associated node types)
	ranges.foreach{ case (r:Range,s:String) =>
		NodeType.makePreterminal(s+"_",'Range)
	}
	sequences.foreach{ case (r:Sequence,s:String) =>
		NodeType.makePreterminal(s+"_",'Sequence)
	}
	durations.foreach{ case (d:Duration,s:String) =>
		NodeType.makePreterminal(s+"_",'Duration)
	}
	
	//----------
	// RULES
	//----------
	private val NAMED_RULES:Array[(GrammarRule,String)] = {
		def hack[A,Z](fn:A=>Z):Any=>Any = fn.asInstanceOf[Any=>Any]
		def hack2[A,B,Z](fn:(A,B)=>Z):(Any,Any)=>Any 
			= fn.asInstanceOf[(Any,Any)=>Any]
		var rtn = List[(GrammarRule,String)]()
		//--Lex
		//(primitives)
		rtn = rtn ::: ranges.map{ case (r:Range,s:String) => 
			val sym = Symbol(s+"_")
			(new TimeLex((sent:Option[Sentence],i:Int) => r, NodeType(sym)
					).restrict( (sent:Sentence,i:Int) => { sent(i) != index.NUM }), s)
		}
		rtn = rtn ::: durations.map{ case (d:Duration,s:String) => 
			val sym = Symbol(s+"_")
			(new TimeLex((sent:Option[Sentence],i:Int) => d, NodeType(sym)
					).restrict( (sent:Sentence,i:Int) => { sent(i) != index.NUM }), s)
		}
		rtn = rtn ::: sequences.map{ case (d:Duration,s:String) => 
			val sym = Symbol(s+"_")
			(new TimeLex((sent:Option[Sentence],i:Int) => d, NodeType(sym)
					).restrict( (sent:Sentence,i:Int) => { sent(i) != index.NUM }), s)
		}
		//(nil)
		rtn = rtn ::: 
			{if(O.lexNils){
				assert(index.wordIndexer.size > 0, "Haven't initialized indexer yet")
				nils.map{ (nil:NodeType) =>
					if(nil.flag('nilnum)){
						(new TimeLex((sent:Option[Sentence],index:Int) => new NIL,
							nil).restrict{ (sent:Sentence,i:Int) =>
								val s = sent.asInstanceOf[TimeSent]
								s.words(i) == index.NUM &&
									getNum(s.nums(i),s.ordinality(i)).name ==
										nil.toString.substring(4)
							},
							"nil-"+nil.toString.substring(4))
					} else {
						val w:Int = index.str2w(nil.toString.substring(4))
						(new TimeLex((sent:Option[Sentence],index:Int) => new NIL,
								nil).restrict( (word:Int) => word == w),
							"nil-"+nil.toString.substring(4))
					}
				}.toList
			} else {
				List[(GrammarRule,String)](
					(new TimeLex((sent:Option[Sentence],index:Int) => new NIL,
						NodeType('NIL)), "nil"))
			}}
		//(numbers)
		rtn = rtn ::: nums.map{ (parent:NodeType) =>
			(new TimeLex(
				(sent:Option[Sentence],index:Int) => sent.get.asNumber(index), 
				parent
				).restrict( (sent:Sentence,i:Int) => {
					val valid = 
						//((is a number))
						sent(i) == index.NUM && 
						//((and is of the right ordinality))
						{sent match {
							case (s:TimeSent) =>
								val len:Int = s.asNumber(i).toString.length-1
								val typ:String = s.ordinality(i).toString
								(parent.flag(Symbol(len.toString)) && parent.flag(Symbol(typ)) ) ||
									parent.flag('largenum)
							case _ => throw new IllegalStateException("Bad sentence")
						}}
					valid
				}),parent.toString)
			}.toList
		//(indexed numbers)
		def thack(fn:Int=>Range, min:Int, max:Int):Any=>Any = (x:Any) =>
			{x match {
				case (num:Int) =>
					if(num >= min && num < max){ fn(num) }
					else { new NoTime }
			}}.asInstanceOf[Any]
		def indirect(first:Boolean,
				head:NodeType,child:NodeType,fn:Any=>Any,nm:String) = {
			List[(GrammarRule,String)](
				(new TimeUnary(fn,NodeType(nm),child),nm)
			) ::: {
				if(first) List[(GrammarRule,String)](
					(new TimeUnary((x:Any) => x,head,NodeType(nm)),
						nm + {if(head == NodeType('Sequence)) ":S" else ":R" }))
				else Nil}
		}
		def indices(first:Boolean,numType:NodeType):List[(GrammarRule,String)]
				= List[List[(GrammarRule,String)]](
			indirect(first,NodeType('Sequence),numType,
				thack((num:Int) =>  MOH(num), 0, 60 ),"moh(n)"),
			indirect(first,NodeType('Sequence), numType,
				thack((num:Int) =>  HOD(num), 0, 24 ), "hod(n)"),
			indirect(first,NodeType('Sequence), numType,
				thack((num:Int) =>  DOM(num), 1, 32 ),"dom(n)"),
			indirect(first,NodeType('Sequence), numType,
				thack((num:Int) =>  MOY(num), 1, 13 ),"moy(n)"),
			indirect(first,NodeType('Sequence), numType,
				thack((num:Int) =>  YOC(num), 0, 100 ),"yoc(n)"),
			indirect(first,NodeType('Sequence), numType,
				thack((num:Int) =>  DOC(num), 0, 10 ),"doc(n)"),
			indirect(first,NodeType('Sequence), numType,
				thack((num:Int) =>  YOD(num), 0, 10 ),"yod(n)"),
			indirect(first,NodeType('Range), numType,
				hack((num:Int) =>  THEYEAR(num) ), "year(n)"),
			indirect(first,NodeType('Range), numType,
				thack((num:Int) =>  CENTURY(num), -100, 100),"century(n)")
			).flatten
		var first:Boolean = true
		nums.foreach{ (numType:NodeType) => {
			rtn = rtn ::: indices(first,numType)
			first = false
		}}
		//--Index
		//(index function)
		rtn = rtn ::: indexedSequences.map{ case (fn:Array[Sequence],name:String) =>
			(new TimeLex( (sent:Option[Sentence],i:Int) => { (n:Int) =>
					if(n < 0 || n >= fn.length){ new NoTime } else { fn(n) }
				}, NodeType("F_{N0th}2S_")
				).restrict( (sent:Sentence,i:Int) => {
					sent(i) != index.NUM
				}),
				name )
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
					hack2((a:Any,fn:Any=>Temporal) => fn(a)),       //function
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
				val rule = (new TimeLex(
							(sent:Option[Sentence],i:Int) => info.fn,
							NodeType("F_{"+a.name+"}2"+a.name+"_")
						).restrict( (sent:Sentence,i:Int) => {
							sent(i) != index.NUM
						}),
					info.name+"$(-:"+a.name+"):"+a.name+"$")
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
					val rule = (new TimeLex(
							(s:Option[Sentence],i:Int) => info.fn,
							NodeType("F_{"+a.name+b.name+"}2"+a.name+"_")
						).restrict( (sent:Sentence,i:Int) => {
							sent(i) != index.NUM
						}),
						info.name+"$(-:"+a.name+",-:"+b.name+"):"+a.name+"$" )
					//(append rule)
					rule :: soFarInner
				}
			}
		}
		
		//--Multiply Duration
		def multiplies(numType:NodeType) = List[(GrammarRule,String)](
			(new TimeBinary(
				hack2( (d:Duration,n:Int) => d*n ),
				NodeType('Duration),
				NodeType('Duration), 
				numType
				), "D*n"),
			(new TimeBinary(
				hack2( (n:Int,d:Duration) => d*n ),
				NodeType('Duration), 
				numType, 
				NodeType('Duration)
				), "n*D")
			)
		nums
				.filter{ (numType:NodeType) => 
					numType.flag(Symbol(NumberType.NUMBER.toString)) ||
					numType.flag(Symbol(NumberType.REAL.toString))    }
				.foreach{ (numType:NodeType) =>
			rtn = rtn ::: multiplies(numType)
		}

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
		rtn = rtn ::: 
				NodeType.all
				.filter{ (x:NodeType) =>
					!x.flag('nil) && x != NodeType.ROOT && x != NodeType.WORD && 
					!x.isPreterminal}
				.foldLeft(List[(GrammarRule,String)]()){
					case (soFar:List[(GrammarRule,String)],term:NodeType) => 
			//(add identities)
			nils.foldLeft(List[(GrammarRule,String)]()){ 
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
	
	val RULES:Array[GrammarRule] = {
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
		
		//--Preterminal Identities
		rtn = rtn ::: NodeType.all
				.filter{ (nt:NodeType) =>
					nt.isPreterminal
				}.map{ (pt:NodeType) =>
					if(pt.flag('Sequence)){
						Some(new TimeUnary((x:Any) => x,NodeType('Sequence),pt))
					} else if(pt.flag('Range)){
						Some(new TimeUnary((x:Any) => x,NodeType('Range),pt))
					} else if(pt.flag('Duration)){
						Some(new TimeUnary((x:Any) => x,NodeType('Duration),pt))
					} else {
						val proposedParent:String
							= pt.toString.substring(0,pt.toString.length-1)
						if(pt.toString.endsWith("_") && NodeType.exists(proposedParent)){
							Some(new TimeUnary((x:Any) => x,NodeType(proposedParent),pt))
						} else {
							None
						}
					}
				}.filter{_.isDefined}.map{ _.get }.toList
//		rtn = rtn ::: NodeType.all
//				.filter{ (nt:NodeType) => 
//					val cand = nt.toString+"_"
//					!nt.isPreterminal && NodeType.exists(cand) }
//				.map{ (nonPreterminal:NodeType) =>
//					val preterminal:NodeType = NodeType(nonPreterminal.toString+"_")
//					new TimeUnary((x:Any) => x,nonPreterminal,preterminal) }
//				.toList
		//--Return
		rtn.toArray
	}
	
	val RULES_STR:Array[String] = {
		val rtn = RULES.map{ _.parent.toString }
		for(i <- 0 until NAMED_RULES.length){
			assert(RULES(i) == NAMED_RULES(i)._1, "name mismatch")
			rtn(i) = NAMED_RULES(i)._2
		}
		rtn
	}

	def r2str(r:CKYRule):String = {
		val cand = RULES.zip(RULES_STR).find( _._1 == r )
		cand match {
			case Some(pair) => pair._2
			case None => r.parent.toString
		}
	}
}

//------------------------------------------------------------------------------
// GROUNDING DATA
//------------------------------------------------------------------------------
class GroundingData(impl:TimeDataset,train:Boolean,index:Indexing
		) extends DataStore[(TimeSent,Temporal,Time)] with Iterable[Annotation] {
	def name:String = "Grounding-"+{if(train) "train" else "eval"}
	def eachExample(i:Int):Iterable[(TimeSent,Temporal,Time)] 
		= impl.goldSpans(train,index)
	def iterator:Iterator[Annotation] 
		= impl.data.iterator.map{ _.impl.asInstanceOf[Annotation] }
}

//------------------------------------------------------------------------------
// GROUNDING TASK
//------------------------------------------------------------------------------
trait TemporalTask {
	def run:Unit
}
@SerialVersionUID(1L)
case class TreeTime(
		parser:CKYParser,
		index:Indexing,
		lex:Lex,
		crf:Option[CRFDetector],
		crfIndex:Option[Indexing]
		) extends Annotator {

	def this(parser:CKYParser,index:Indexing,lex:Lex)
		= this(parser,index,lex,None,None)

	def addDetector(detector:CRFDetector,detectorIndex:Indexing):TreeTime = {
		new TreeTime(parser,index,lex,Some(detector),Some(detectorIndex))
	}

	private def parse(sent:TimeSent,ground:Time):(Temporal,Temporal,Double) = {
		//(run parser)
		var time:Temporal = new NoTime
		var originalTime:Temporal = new NoTime
		var logProb:Double = Double.NegativeInfinity
		var beam:Int = 1
		while(time.isInstanceOf[NoTime] && beam <= 128){
			//((parse))
  		val parses:Array[EvalTree[Any]] = parser.parse(sent.reIndex(index),beam)
			//((get candidate))
			val (t,ot,lP) = parses.slice(beam/2-1,beam)
					.foldLeft((
						(new NoTime).asInstanceOf[Temporal],
						(new NoTime).asInstanceOf[Temporal],
						Double.NegativeInfinity)){ 
					case ((soFar:Temporal,orig:Temporal,lProb:Double),parse:EvalTree[Any]) =>
				if(soFar.isInstanceOf[NoTime]){
					val rawTime = parse.evaluate.asInstanceOf[Temporal]
					val timeDist = rawTime.distribution(Range(ground,ground))
					if(timeDist.hasNext){
						val (evaluated,timeProb,offset) = timeDist.next
						(evaluated, rawTime, parse.logProb + math.log(timeProb)) //<--found a parse
					} else {
						(soFar,orig,lProb) //<--evaluated to NoTime
					}
				} else {
					(soFar,orig,lProb) //<--early exit
				}
			}
			//((set variables))
			time = t
			originalTime = ot
			logProb = lP
			//((increment beam))
			beam *= 2
		}
		(time,originalTime,logProb)
	}

	private def toStanfordTimex(time:Temporal,original:Temporal):StanfordTimex = {
		//(get timex data)
		import JodaTimeUtils._
		val (typ,value) = time match {
			case (n:NoTime) => 
				("MISS","?")
			case (gr:GroundedRange) => 
				if(original.isInstanceOf[UngroundedRange] &&
						original.asInstanceOf[UngroundedRange].isRef){
					("DATE","PRESENT_REF")
				} else {
					val opts = new JodaTimeUtils.ConversionOptions
					opts.forceDate = false
					("DATE",timexDateValue(gr.begin.base,gr.end.base,opts))
				}
			case (d:GroundedDuration) => 
				val opts = new JodaTimeUtils.ConversionOptions
				opts.approximate = false
//				opts.forceUnits = d.timexUnits
				opts.forceUnits = Array[String]("L","C","Y","M")
				("DURATION",timexDurationValue(d.base,opts))
			case (d:FuzzyDuration) => 
				val opts = new JodaTimeUtils.ConversionOptions
//				opts.forceUnits = d.timexUnits
				opts.forceUnits = Array[String]("L","C","Y","M")
				opts.approximate = true
				("DURATION",timexDurationValue(d.interval.base,opts))
			case (t:Time) => ("TIME",timexTimeValue(t.base))
			case _ => throw new IllegalStateException("Unknown time: " + time.getClass)
		}
		//(create timex)
		new StanfordTimex(typ,value)
	}

	private def annotateKnownSpan(tokens:JList[CoreLabel], expr:CoreMap,
			ground:Time) = {
		//--Parse
		//(create sentence)
		val sent = DataLib.mkTimeSent(expr,tokens,index,false)
		//(parse)
		val (time,original,logProb) = parse(sent,ground)
		//--Annotate
		val timex = toStanfordTimex(time,original)
		log("annotated "+sent+" as "+timex)
		//(get span)
		val begin:Int = expr.get[JInt,BeginIndexAnnotation](classOf[BeginIndexAnnotation])
		val end:Int = expr.get[JInt,EndIndexAnnotation](classOf[EndIndexAnnotation])
		//(annotate)
		(begin until end).foreach{ (i:Int) =>
			tokens.get(i).set(classOf[TimexAnnotation], timex)
		}
	}
	
	private def annotateSentence(tokens:JList[CoreLabel], ground:Time) = {
		//--Run CRF
		val sent = DataLib.mkTimeSent(tokens,crfIndex.get,false)
		val times:Array[DetectedTime] = crf.get.findTimes(sent,
			(s:TimeSent) => {
				crfIndex.map{ (crfInd:Indexing) =>
					parse(s.reIndex(crfInd),ground)
				}
			})
		//--Annotate
		times.foreach{ case DetectedTime(begin,end,timeInfo) =>
			val (time,original,logProb) = timeInfo.get
			val timex = toStanfordTimex(time,original)
			log("detected " + 
				tokens.slice(begin,end).map{ _.word }.mkString(" ") + 
				" as " + timex)
			(begin until end).foreach{ (i:Int) =>
				tokens.get(i).set(classOf[TimexAnnotation], timex)
			}
		}
	}
	
	//<<annotator overrides>>
	override def annotate(ann:Annotation){
		//--Variables
		val ground = new Time(new DateTime(ann
				.get[Calendar,CalendarAnnotation](classOf[CalendarAnnotation])
				.getTimeInMillis ))
		//--Cycle Sentences
		ann
				.get[JList[CoreMap],SentencesAnnotation](classOf[SentencesAnnotation])
				.foreach{ (sent:CoreMap) =>
			//(variables)
			val tokens = sent
					.get[JList[CoreLabel],TokensAnnotation](classOf[TokensAnnotation])
			if(crf.isDefined){
				//(annotate sentence)
				assert(crfIndex.isDefined, "CRF but no CRF Index found!")
				annotateSentence(tokens,ground)
			} else if(sent.containsKey[JList[CoreMap],TimeExpressionsAnnotation](
					classOf[TimeExpressionsAnnotation])){
				//(annotate gold spans)
				val knownSpans = 
					sent.get[JList[CoreMap],TimeExpressionsAnnotation](classOf[TimeExpressionsAnnotation])
				if(knownSpans != null){
					knownSpans.foreach{ (expr:CoreMap) =>
						annotateKnownSpan(tokens,expr,ground)
					}
				}
			} else {
				//(nothing to annotate)
				throw new IllegalStateException("Cannot annotate without CRF detector")
			}
		}
	}
}



class InterpretationTask extends TemporalTask {
	//--Initialize JodaTime
	log("JodaTime settings")
	DateTimeZone.setDefault(DateTimeZone.UTC);
	//--Create Data
	forceTrack("loading dataset")
	val (data,index) = {
		//(raw dataset)
		val rawDataset = new TimeDataset(new SerializedCoreMapDataset(
			System.getenv("HOME") + 
				"/workspace/time/aux/coremap/tempeval2-english-retok-numbers"
			))
		val eval = if(O.devTest) O.dev else O.test
		//(create data)
		if(O.train.source == O.DataSource.Toy || eval.source == O.DataSource.Toy){
		  (ToyData.STANDARD,ToyData.index)
		} else {
			val index = Indexing()
		  (TimeData(
				new GroundingData( rawDataset.slice(O.train.begin,O.train.end),
					true, index),
				new GroundingData( rawDataset.slice(eval.begin,eval.end),
					false, index)
			),
			index)
		}
	}
	forceTrack("NOOP loop")
	data.noopLoop
	endTrack("NOOP loop")
	endTrack("loading dataset")
	//--Create Parser
	startTrack("Creating Grammar")
	val lex = new Lex
	import lex._
	val grammar = new Grammar(index,lex,NodeType.defaultFactory)
	endTrack("Creating Grammar")

	//<<scoring>>
	case class GoodOutput(tree:EvalTree[Any],value:Temporal,
			offset:Long,prob:Double,ground:Time,sent:TimeSent) {
		assert(prob >= 0 && prob <= 1.0, "Invalid probability: " + prob)
		def logProb:Double = math.log(prob)
		def normalize(total:Double,count:Int):GoodOutput = {
			if(total == 0.0){
				GoodOutput(tree,value,offset,1.0/count.asInstanceOf[Double],ground,sent)
			} else {
				GoodOutput(tree,value,offset,prob/total,ground,sent)
			}
		}
	}
	case class CompareElem(offset:Int,diff:(Duration,Duration),prob:Double)
	case class ScoreElem(index:Int,offset:Int,diff:(Duration,Duration),
			logProb:Double, temporal:Temporal){
		def exact:Boolean = { U.sumDiff(diff) < O.exactMatchThreshold }
	}
	
	def compare(guessRaw:Temporal, gold:Temporal,ground:GroundedRange
			):Iterator[CompareElem] = {
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
		val rtn = guessRaw.distribution(ground).map{
				case (guess:Temporal,prob:Double,offset:Long) =>
			//(get diff)
			val d = diff(gold,guess,false)
//			//(check timex consistency)
//			if(U.sumDiff(d) > O.exactMatchThreshold){
//				import edu.stanford.nlp.time.JodaTimeUtils._
//				val (tGold, tGuess) = (gold,guess) match {
//					case (a:UnkTime, b:Temporal) => {("not", "equal")}
//					case (a:Temporal, b:NoTime) => {("not", "equal")}
//					case (a:GroundedRange,b:GroundedRange) => {
//						(timexDateValue(a.begin.base, a.end.base),
//							timexDateValue(b.begin.base,b.end.base))
//					}
//					case (a:FuzzyDuration,b:FuzzyDuration) => {
//						(timexDurationValue(a.interval.base,true),
//							timexDurationValue(b.interval.base,true))
//					}
//					case (a:GroundedDuration,b:GroundedDuration) => {
//						(timexDurationValue(a.interval.base),
//								timexDurationValue(b.interval.base))
//					}
//					case _ => ("not","equal")
//				}
//				if(tGold.equals(tGuess)){
//					log("Timexes match but " +
//						"difference is nonzero: gold="+tGold+" guess="+tGuess+
//						"  myGuess="+guess+"  inferredGold="+gold+" (diff="+d+") :: ")
//				}
//			}
//			//(debug)
//			assert(O.timeDistribution != O.Distribution.Point || //TODO enable me
//				offset == 0L ||
//				prob == 0.0,
//				"Time returned distribution when it shouldn't have: " 
//					+ guessRaw + " (offset=" + offset + ") [prob=" + prob + "]")
			//(return)
			CompareElem(offset.toInt,d,prob)
		}
		rtn
	}

	def filterCorrect(correct:Array[GoodOutput]):Iterable[GoodOutput] = {
		//--Utility Functions
		def countNonNils(tree:ParseTree,sent:Sentence):(Int,Int) = {
			var words = List[Int]()
			var tags = List[CKYUnary]()
			tree.traverse( 
				(rule:CKYRule) => {}, 
				(rule:CKYUnary,w:Int) => { 
					tags = rule :: tags 
					words = w :: words 
				} )
			val nonNilCount:Int = tags.filter(_.parent.flag('nil)).length
			val trimmedLength:Int = words.zip(tags)
				.dropWhile{ case (w,t) => t.parent.flag('nil)}
				.reverse
				.dropWhile{ case (w,t) => t.parent.flag('nil)}
				.map{_._1}
				.length
			(trimmedLength,nonNilCount)
		}
		def trim(tree:ParseTree,sent:Sentence):Array[Int] = {
			var words = List[Int]()
			var tags = List[CKYRule]()
			tree.traverse( 
				(rule:CKYRule) => {}, 
				(rule:CKYUnary,w:Int) => { 
					tags = rule :: tags 
					words = w :: words 
				} )
			words.zip(tags)
				.dropWhile{ case (w,t) => t.parent.flag('nil)}
				.reverse
				.dropWhile{ case (w,t) => t.parent.flag('nil)}
				.map{_._1}
				.toArray
		}
		//--Filter
		val good:Iterable[GoodOutput] = O.ckyCountType match {
			case O.CkyCountType.all => 
				//(all parses valid)
				val total:Double = correct.map{_.prob}.sum
				correct.map{ _.normalize(total,correct.length) }
			case O.CkyCountType.bestAll => 
				//(all best-scoring parses valid)
				val maxProb = correct.maxBy( _.prob ).prob
				val ok = correct.filter( _.prob == maxProb )
				val total:Double = ok.map{_.prob}.sum
				ok.map{ _.normalize(total,ok.length) }
			case O.CkyCountType.bestRandom => 
				//(single best scoring parse valid)
				val maxProb = correct.maxBy( _.prob ).prob
				val ok = correct.filter( _.prob == maxProb )
				if(ok.length == 0){
					Array[GoodOutput]()
				} else {
					Array[GoodOutput](ok(0).normalize(ok(0).prob,1))
				}
			case O.CkyCountType.shortWithOffsetZero => {
				//((has an offset zero term?))
				val hasZeroOffset:Boolean = correct.exists{ _.offset == 0L }
				//((get shortest length))
				val shortest:Int = correct.foldLeft(Int.MaxValue){
						case (shortest:Int,output:GoodOutput) =>
					if(hasZeroOffset && output.offset == 0L){
						math.min(shortest, trim(output.tree,output.sent).length)
					} else {
						shortest
					}
				}
				//((get matching trees))
				val matching:Array[GoodOutput] = correct.filter{
						(output:GoodOutput) =>
					trim(output.tree,output.sent).length <= shortest &&
						(!hasZeroOffset || output.offset == 0L)
				}
				//((create list))
				val total:Double = matching.map{_.prob}.sum
				matching.map{ _.normalize(total,matching.length) }
			}
			case O.CkyCountType.mostNilsWithOffsetZero => {
				//((has an offset zero term?))
				val hasZeroOffset:Boolean = correct.exists{ _.offset == 0L }
				//((get shortest length))
				val shortest:(Int,Int) = correct
						.foldLeft( (Int.MaxValue,Int.MaxValue) ){
						case ((shortestTrim:Int,nonNils:Int),output:GoodOutput) =>
					if(hasZeroOffset && output.offset == 0L){
						val (trim,nonNilCount) = countNonNils(output.tree,output.sent)
						if(trim == shortestTrim){
							(trim,math.min(nonNils,nonNilCount))
						} else if(trim < shortestTrim){
							(trim,nonNilCount)
						} else {
							(shortestTrim,nonNils)
						}
					} else {
						(shortestTrim,nonNils)
					}
				}
				//((get matching trees))
				val (shortestTrim,nonNils) = shortest
				val matching:Array[GoodOutput] = correct.filter{
						(output:GoodOutput) =>
					val (trim,nonNilCount) = countNonNils(output.tree,output.sent)
					trim <= shortestTrim && nonNilCount <= nonNils &&
						(!hasZeroOffset || output.offset == 0)
				}
				//((create list))
				val total:Double = matching.map{_.prob}.sum
				matching.map{ _.normalize(total,matching.length) }
			}
			case _ => throw fail("Unknown case: " + O.ckyCountType)
		}
		//--Return
		//(debug)
		assert(correct.length == 0 || good.exists( _.prob > 0.0 ), 
			"All outputs have zero mass" )
		//(return)
		good
	}

	def handleParses(
			parses:Array[EvalTree[Any]],
			gold:Temporal,
			grounding:Time,
			score:Score,
			sent:TimeSent
			):Iterable[GoodOutput] = {
		//--Util
		val viterbi:Option[Temporal] = 
			if(parses != null && parses.length > 0) {
				val parse:Any = parses(0).evaluate
				parse match {
					case (time:Temporal) => Some(time(grounding))
					case _ => throw new IllegalStateException("Not a temporal: " + parse)
				}
			} else {
				None
			}
		//--Score Parses
		val scoresUnsorted:Array[ScoreElem]
			//(for each parse...)
			= parses.zipWithIndex.foldLeft((List[ScoreElem](),false)){ 
					case ((soFar:List[ScoreElem],isPruned:Boolean),
					      (parse:EvalTree[Any],i:Int)) => 
				if(!isPruned){
					//(variables)
					val ground:GroundedRange 
						= if(O.guessRange){ grounding.guessRange }
						  else{ Range(grounding,grounding) }
					val parseProb = parse.logProb
					//(timing)
					val parseWatch:Stopwatch = new Stopwatch
					parseWatch.start
					//(debug variables)
					var lastProb = Double.PositiveInfinity
					//(get guess temporal)
					val guess:Temporal = parse.evaluate match {
						case (t:Temporal) => t
						case _ => throw new IllegalStateException("Not a temporal: "+parse)
					}
					//(for each offset of parse...)
					val compareRes = compare(guess,gold,ground).slice(0,O.scoreBeam).toList
					val totalProb:Double = compareRes.foldLeft(0.0){ _ + _.prob }
					val rtn = soFar ::: compareRes.map{ (elem:CompareElem) =>
							val prob = elem.prob / totalProb
							//(debug)
//							assert(prob <= lastProb,  //TODO enable me
//								"Times are not monotonically decreasing: "+lastProb+"->"+
//									prob+" from " + guess)
							lastProb = prob
							//(create parse)
							val resultLogProb = 
								if(O.includeTimeProb){ parse.logProb+math.log(prob) }
								else{ parse.logProb }
							ScoreElem(i,elem.offset,elem.diff,resultLogProb,guess)
						}
					//(timing & return)
					val lapTime = parseWatch.lap
					if(lapTime > O.pruneTime && i > O.pruneMinIndex){
						log(FORCE,"pruning after " + i)
						(rtn,true)
					} else {
						(rtn,false)
					}
				} else {
					(soFar,isPruned)
				}
		}._1.toArray
		val scores = 
			if(O.sortTimeProbInScore){
				scoresUnsorted.sortWith{ case (a:ScoreElem,b:ScoreElem) => 
					//(case: order by P(parse)*P(time))
					if( (b.logProb - a.logProb).abs < 1e-6 ){
						if(a.index == b.index){
							b.offset.abs > a.offset.abs
						} else {
							b.index > a.index
						}
					} else {
						b.logProb < a.logProb 
					}
				}
			} else {
				scoresUnsorted
			}
		log("" + scores.length + " candidates")
		//--Score Parses
		score.lock.acquire
		val isCorrect:Boolean = if(scores.length == 0){
			//(case: no scores)
			score.enter(false,(Duration.INFINITE,Duration.INFINITE), -1)
			score.store(sent,viterbi.orNull,gold,false,grounding)
			score.logFailure(sent,gold,grounding)
			false
		} else {
			//(case: have score)
			//((get guess))
			val bestGuess = scores(0)
//			assert(O.timeDistribution != O.Distribution.Point ||  //TODO enable me
//				bestGuess.offset == 0,
//				"Sanity check for time distribution")
			//((is in beam?))
			val correct:Array[ScoreElem] = scores.filter{ (elem:ScoreElem) => 
				assert(!elem.logProb.isNaN && elem.logProb <= 0.0, 
					"invalid probability")
				elem.exact }
			//((enter score))
			if(bestGuess.exact){
				log(GREEN,"CORRECT")
			} else {
				log(FORCE,"best guess: " + bestGuess)
				if(bestGuess.offset != 0){
					log(FORCE,"then: " + scores(1))
				}
				log(FORCE,RED,"WRONG")
			}
			score.enter(bestGuess.exact,bestGuess.diff, 
				if(correct.length > 0) correct(0).index else -1)
			score.enterK(scores.slice(0,O.reportK).map{ _.exact })
			score.store(sent,viterbi.get,gold,bestGuess.exact,grounding)
			if(correct.length == 0){
				log(FORCE,RED,"" + correct.length + " in beam")
			} else {
				log("" + correct.length + " in beam")
			}
			bestGuess.exact
		}
		//--Log
		log({if(isCorrect) FORCE else null },"Guess:  " + viterbi.orNull)
		log({if(isCorrect) FORCE else null },"Gold:   " + gold)
		log({if(isCorrect) FORCE else null },"Tree:   " + {
			if(parses.length > 0){
				parses(0).asParseString(index.w2str(_),grammar.r2str(_)) 
			} else{ "" } })
		log({if(isCorrect) FORCE else null },"Ground: " + grounding)
		score.lock.release
		//--Post-Filter
		def allOutput:Iterable[GoodOutput] = scores.map{ (elem:ScoreElem) =>
			if(elem.exact){
				Some(GoodOutput(
					parses(elem.index),
					elem.temporal,
					elem.offset,
					math.exp(elem.logProb),
					grounding,
					sent))
			} else {
				None
			}
		}.filter{ _.isDefined }.map{ _.get }
		val filtered = filterCorrect(allOutput.toArray)
		//--Return
		//(debug)
		if(isCorrect){
			startTrack("Good Output")
		} else {
			forceTrack("Good Output")
		}
		filtered.foreach{ (t:GoodOutput) => 
			log("["+t.offset+"] "+
				"("+G.df.format(t.prob)+")"+
				t.tree.asParseString(index.w2str(_),grammar.r2str(_))) 
		}
		endTrack("Good Output")
		//(return)
		filtered
	}

	override def run:Unit = {
		//--Run
		forceTrack("Running")
		//(train)
		startTrack("Training")
		forceTrack("Creating Parser")
		val initialParser = CKYParser(index.W,grammar.RULES)
		endTrack("Creating Parser")
		log("Threading on " + Execution.numThreads + " threads")
		val (parser,trainScoresRev):(CKYParser,List[Score]) 
			= if(O.runInterpretModel){
				(0 until O.iters).foldLeft( (initialParser,List[Score]()) ){
						case ((parser:CKYParser,scores:List[Score]),iter:Int) =>
				forceTrack("Iteration " + iter)
				//(create score)
				val score = new Score
				var goodParses:List[GoodOutput] = List[GoodOutput]()
				val parseLock = new scala.concurrent.Lock()
				//(create tasks)
				val tasks:java.lang.Iterable[Runnable] = asJavaIterable(
					data.train.eachExample(iter).zipWithIndex.map{
							case ((sent:TimeSent,gold:Temporal,ground:Time),i:Int) =>
					new Runnable{ override def run {
						startTrack("[timex " + i + "] " + sent.toString)
						//((parse))
						val parses:Array[EvalTree[Any]] = parser.parse(sent,O.beam)
						//((handle parses))
						val filteredParses = handleParses(parses, gold, ground, score, sent)
						//((store parses))
						endTrack("[timex " + i + "] " + sent.toString)
						parseLock.acquire
						goodParses = goodParses ::: filteredParses.toList
						parseLock.release
					}}
				})
				//(run tasks)
				threadAndRun("Parsing", tasks, Execution.numThreads)
				//(debug)
				assert(goodParses.toArray.length == 0 || 
					goodParses.exists( _.prob > 0.0 ), 
					"All good parses have zero mass" )
				log("finished parsing")
				//(filter prob>0 parses)
				val nonZeroGoodParses = goodParses
						.filter{ _.logProb > Double.NegativeInfinity }
				//(update parser)
				val newParser = parser.update(
					nonZeroGoodParses
						.map{ (o:GoodOutput) => 
							new ReweightedParseTree(o.tree,o.logProb) 
						}
					)
				log("updated parser")
				//(update time)
				startTrack("Updating Times")
				//((E-step))
				nonZeroGoodParses.foreach{ 
						case GoodOutput(tree,value,offset,prob,ground,s) =>
					val gr:GroundedRange = Range(ground,ground)
					assert(prob > 0.0 && prob <= 1.0, "Bad time probability: " + prob)
//					forceTrack(tree.asParseString(index.w2str(_),grammar.r2str(_)))
					value.traverse(gr,offset,
						(term:Temporal,trueOffset:Long) => {
//							log(FORCE,"Touching " + term + " " + term.getClass)
							term.updateE(gr,trueOffset,if(O.hardEM) 0 else U.safeLn(prob))
						}
					)
//					endTrack(tree.asParseString(index.w2str(_),grammar.r2str(_)))
				}
				//((M-step))
				nonZeroGoodParses.foreach{ 
						case GoodOutput(tree,value,offset,prob,ground,s) =>
					val gr:GroundedRange = Range(ground,ground)
					value.traverse(gr,offset,
						(term:Temporal,trueOffset:Long) => term.runM)
				}
				grammar.ranges.foreach{ _._1.runM }
				grammar.durations.foreach{ _._1.runM }
				grammar.sequences.foreach{ _._1.runM }
				endTrack("Updating Times")
				//(debug)
				startTrack("Parameters")
				Execution.touchAndWrite("params-"+iter, 
					newParser.parameters(index.w2str(_),grammar.r2str(_)) )
				endTrack("Parameters")
				//(continue loop)
				log(FORCE,BOLD,YELLOW,""+score)
				log(FORCE,YELLOW,""+score.reportK)
				endTrack("Iteration " + iter)
				(newParser,score :: scores)
			}
		} else {
			val parser:CKYParser = 
				try {
					log("Loading parser at: " + O.interpretModel)
					IOUtils.readObjectFromFile(O.interpretModel)
					.asInstanceOf[TreeTime]
					.parser
				} catch {
					case (e:Throwable) => throw new RuntimeException(e)
				}
			val trainScoresRev:List[Score] = List[Score]()
			(parser,trainScoresRev)
		}
		endTrack("Training")
		//(test)
		startTrack("Eval")
		val testScore = new Score
		data.eval.eachExample(Int.MaxValue).zipWithIndex.foreach {
					case ((sent:TimeSent,gold:Temporal,ground:Time),i:Int) =>
			startTrack("[" + i + "] "+{if(O.devTest){ sent.toString } else { "" }})
			//((parse))
			val parses:Array[EvalTree[Any]] = parser.parse(sent,O.beam)
			//((handle parses))
			handleParses(parses, gold, ground, testScore, sent)
			//((continue map))
			endTrack("[" + i + "] " + {if(O.devTest){ sent.toString } else { "" }})
		}
		log(FORCE,BOLD,YELLOW,""+testScore)
		log(FORCE,YELLOW,""+testScore.reportK)
		endTrack("Eval")
		endTrack("Running")
		//--Score
		startTrack(FORCE,BOLD,"Results")
		val sys = new TreeTime(parser,index,lex)
		reportScores(sys,trainScoresRev.reverse.toArray,testScore)
		endTrack("Results")
		//--Save Model
		if(O.interpretModel != null){
			try {
				import org.goobs.util.TrackedObjectOutputStream
				import java.io.FileOutputStream
				IOUtils.writeObjectToFile(sys,Execution.touch("interpretModel.ser.gz"))
			} catch {
				case (e:Throwable) => throw new RuntimeException(e)
			}
		}
	}

	def reportScores(sys:TreeTime,trainScores:Array[Score],testScore:Score) {
		val logger = Execution.getLogger();
		//--External Score
		if(O.train.source == O.DataSource.English) {
			startTrack("TempEval")
			//(run)
			startTrack("Train")
			val trn = Entry.officialEval(sys,data.train.asInstanceOf[Iterable[Annotation]],
				true,O.tempevalHome,Execution.touch("attr-train.tab"))
			endTrack("Train")
			startTrack("Eval")
			val tst = Entry.officialEval(sys,data.eval.asInstanceOf[Iterable[Annotation]],
				false,O.tempevalHome,Execution.touch("attr-test.tab"))
			endTrack("Eval")
			//(print)
			startTrack("Eval Results")
			log(FORCE,BOLD,GREEN,"TreeTime Train:     " + trn)
			logger.setGlobalResult("train.tempeval.type", trn.typeAccuracy)
			logger.setGlobalResult("train.tempeval.value", trn.valueAccuracy)
			log(FORCE,BOLD,GREEN,"TreeTime Eval:      " + tst)
			logger.setGlobalResult("test.tempeval.type", tst.typeAccuracy)
			logger.setGlobalResult("test.tempeval.value", tst.valueAccuracy)
			endTrack("Eval Results")
			endTrack("TempEval")
		}
		//--Process
		//(train)
		if(trainScores.length > 0){
			startTrack(BOLD,"train")
			logger.setGlobalResult("train.accuracy",
				trainScores(trainScores.length-1).accuracy)
			logger.setGlobalResult("train.averank",
				trainScores(trainScores.length-1).avePos)
			logger.setGlobalResult("train.inbeam",
				trainScores(trainScores.length-1).percentParsable)
			logger.setGlobalResult("train.score",
				trainScores(trainScores.length-1).aveScore())
			log(FORCE,BOLD,YELLOW,"train.accuracy: " + 
				trainScores(trainScores.length-1).accuracy)
			log(FORCE,YELLOW,"train.averank: " +	
				trainScores(trainScores.length-1).avePos)
			log(FORCE,YELLOW,"train.inbeam: " + 
				trainScores(trainScores.length-1).percentParsable)
			log(FORCE,YELLOW,"train.score: " + 
				trainScores(trainScores.length-1).aveScore())
			endTrack("train")
		}
		//(test)
		val s = if(O.devTest) "dev" else "test"
		startTrack(BOLD,s)
		logger.setGlobalResult(s+".accuracy", testScore.accuracy)
		logger.setGlobalResult(s+".averank", testScore.avePos)
		logger.setGlobalResult(s+".inbeam", testScore.percentParsable)
		logger.setGlobalResult(s+".score", testScore.aveScore())
		log(FORCE,BOLD,YELLOW,s+".accuracy: "+ testScore.accuracy)
		log(FORCE,YELLOW,s+".averank: "+ testScore.avePos)
		log(FORCE,YELLOW,s+".inbeam: "+ testScore.percentParsable)
		log(FORCE,YELLOW,s+".score: "+ testScore.aveScore())
		endTrack(s)
		//--Debug dump
		log("saving parses")
		trainScores.zipWithIndex.foreach( pair => {
			val (score,i) = pair
			val writer = new java.io.FileWriter(Execution.touch("train"+i))
			score.results.foreach( r => {
				writer.write(r.toString); writer.write("\n")
			})
			writer.close
		})
		val writer = new java.io.FileWriter(Execution.touch(s))
		testScore.results.foreach( r => {
			writer.write(r.toString); writer.write("\n")
		})
		writer.close
	}
}

//------------------------------------------------------------------------------
// AUXILLIARY CLASSES
//------------------------------------------------------------------------------
case class UNK()

object Score {
	def duration2double(d:Duration)
		= d.seconds.asInstanceOf[Double] / O.scoreGranularity.asInstanceOf[Double]
	def score(
			diff:(Double,Double), 
			c_over:Double = O.c_overconstraining,
			c_vague:Double = O.c_vagueness ):Double = {
		val (start,end) = diff
		val cStart:Double = if(start < 0){ c_over } else { c_vague }
		val cEnd:Double = if(end < 0){ c_vague } else { c_over }
		val scoreStart:Double = (cStart / (cStart + start.abs))
		val scoreEnd:Double = (cEnd / (cEnd + end.abs))
		assert(scoreStart >= 0 && scoreStart <= 1.0, "Start score must be in range")
		assert(scoreEnd >= 0 && scoreEnd <= 1.0, "End score must be in range")
		if(scoreStart < 1e-10 && scoreEnd < 1e-10) {
			0.0 //score of zero
		} else {
			(scoreStart+scoreEnd)/2.0
		}
	}
	def score(diff:(Duration,Duration)):Double = {
		score( (duration2double(diff._1),duration2double(diff._2)) )
	}
}

class Score {
	case class Result(sent:TimeSent,guess:Temporal,gold:Temporal,exact:Boolean,
			ground:Time) {
		override def toString:String = {
			"" + {if(exact) "HIT  " else if(guess != null) "MISS " else "FAIL "} +
			sent + " as " + 
			{if(guess != null) guess else "<fail>"} + 
			" <gold " + gold + ">"
		}
	}
	private var exactRight:Int = 0
	private var exactRightK:Array[Int] = new Array[Int](O.reportK)
	private var total:Int = 0
	private var sumPos = 0
	private var totalWithPos = 0
	private var totalWithNonzeroPos = 0
	private var goldMinusGuess = List[(Double,Double)]()
	private var resultList:List[Result] = List[Result]()
	private var failedList = List[(TimeSent,Temporal,Time)]()
	val lock = new scala.concurrent.Lock

	def releaseResults:Unit = { resultList = List[Result]() }
	def enter(exact:Boolean,diff:(Duration,Duration), position:Int) = {
		//(score exact)
		if(exact){ exactRight += 1 }
		total += 1
		//(store differences)
		goldMinusGuess
			= (Score.duration2double(diff._1), Score.duration2double(diff._2)) ::
				goldMinusGuess
		//(score position)
		if(position > 0){
			sumPos += position
			totalWithNonzeroPos += 1
		}
		if(position >= 0){
			totalWithPos += 1
		}
	}
	def enterK(topK:Array[Boolean]) = {
		assert(topK.length <= O.reportK, "Entering too many parses")
		(0 until exactRightK.length).foreach{ (i:Int) =>
			val anyOK:Boolean = !topK.slice(0,i+1).forall( !_ )
			if(anyOK){ exactRightK(i) += 1 }
		}
	}
	def store(sent:TimeSent,guess:Temporal,gold:Temporal,exact:Boolean,
			ground:Time)={
		resultList = Result(sent,guess,gold,exact,ground) :: resultList
	}
	def logFailure(sent:TimeSent,gold:Temporal,ground:Time) = {
		failedList = (sent,gold,ground) :: failedList
	}

	def accuracy:Double 
		= exactRight.asInstanceOf[Double]/total.asInstanceOf[Double]
	def avePos:Double 
		= sumPos.asInstanceOf[Double] / totalWithNonzeroPos.asInstanceOf[Double]
	def percentParsable:Double
		= (totalWithPos.asInstanceOf[Double] / total.asInstanceOf[Double])
	def aveScore(
			overconstrainingPenalty:Double = O.c_overconstraining, 
			vaguenessPenalty:Double = O.c_vagueness) = {
		val sum = goldMinusGuess.foldLeft(0.0)( 
			(soFar:Double,diff:(Double,Double)) => {
				val score = Score.score(diff,overconstrainingPenalty,vaguenessPenalty)
				assert(score == 0.0 || soFar+score > soFar, "Double overflow")
				soFar + score
			})
		sum / goldMinusGuess.length.asInstanceOf[Double]
	}
	def results:Array[Result] = resultList.reverse.toArray

	def reportK:String = {
		"beam quality: " + exactRightK.map{ (count:Int) =>
			G.df.format(count.asInstanceOf[Double] / total.asInstanceOf[Double]) 
		}.mkString("  ")
	}
	def reportFailures(y:(String=>Any)):Unit = {
		failedList.foreach{ case (sent:TimeSent,gold:Temporal,ground:Time) =>
			y(sent.toString + " :: " + gold + " :: " + ground)
		}
	}

	override def toString:String = {
		"accuracy: "+G.df.format(accuracy)+"; average pos: "+G.df.format(avePos)+
			" (in " + G.pf.format((percentParsable*100)) + "%); score: "+
			G.df.format(aveScore())
	}
}
//------------------------------------------------------------------------------
// TOY DATA(S)
//------------------------------------------------------------------------------
object ToyData {
	val lex = new Lex
	val index = Indexing()
	import lex._

	private val toys = new HashMap[String,Int]

	private val NONE = ToyStore(Array[(String,Temporal)](),false)
	private def store(test:Boolean,args:(String,Temporal)*):ToyStore 
		= ToyStore(args.toArray,test)
	//--Toy
	private val thisMorning = ("this morning",(TOD(1)))
	private val today = ("today",(TODAY))
	private val day = ("day",(DAY(todaysDate)))
	private val week = ("week",(WEEK(todaysDate)))
	private val aWeek = ("a week",(AWEEK))
	private val theWeek = ("the week",(WEEK(todaysDate)))
	private val thisWeek = ("this week",(REF ! AWEEK))
	private val lastWeekToday = ("last week today",(WEEK move -1))
	private val lastWeekNow = ("last week now",(WEEK move -1))
	private val lastWeek = ("last week",(WEEK move -1))
	private val aWeekAgo = ("a week ago",(Range(todaysDate,todaysDate) << AWEEK))
	private val weekLast = ("week last",(WEEK move -1))
	private val pastWeek = ("past week",(REF <| AWEEK))
	private val thePastWeek = ("the past week",(REF <| AWEEK))
	private val pastMonths2 = ("past 2 months",(REF <| (AMONTH*2)))
	private val pastYear = ("past year",(REF <| AYEAR))
	private val weeks2 = ("2 weeks",(AWEEK*2))
	private val months2 = ("2 months",(AMONTH*2))
	private val monthsdash2 = ("2 - month",(AMONTH*2))
	private val years2 = ("2 years",(AYEAR*2))
	private val year5 = ("5 years",(AYEAR*5))
	private val year5AndJunk = ("5 years in 2",(AYEAR*5))
	private val weeksDash2 = ("2 - weeks",(AWEEK*2))
	private val week2Period = ("2 week period",(AWEEK*2))
	private val month = ("month",(MONTH))
	private val aMonth = ("a month",(AMONTH))
	private val theMonth = ("the month",(MONTH))
	private val lastMonth = ("last month",(MONTH move -1))
	private val nextMonth = ("next month",(MONTH move 1))
	private val thisMonth = ("this month",(MONTH))
	private val spring = ("spring",(SEASON(1)))
	private val summer = ("summer",(SEASON(2)))
	private val fall = ("fall",(SEASON(3)))
	private val winter = ("winter",(SEASON(4)))
	private val quarter = ("quarter",(QUARTER))
	private val aQuarter = ("a quarter",(AQUARTER))
	private val lastQuarter = ("last quarter",(QUARTER move -1))
	private val firstQuarter =  ("1st quarter",(QOY(1)))
	private val secondQuarter = ("2st quarter",(QOY(2)))
	private val thirdQuarter = ( "3st quarter",(QOY(3)))
	private val fourthQuarter = ("4st quarter",(QOY(4)))
	private val y1776 = ("1776",(THEYEAR(1776)))
	private val y17sp76 = ("17 76",(THEYEAR(1776)))
	private val april = ("april",(MOY(4)))
	private val april1776 = ("april 1776",(MOY(4) ^ THEYEAR(1776)))
	private val april2 = ("april 2",(MOY(4) ^ DOM(2)))
	private val year = ("year",(YEAR))
	private val ayear = ("a year",(AYEAR))
	private val lastYear = ("last year",(YEAR move -1))
	private val thisYear = ("this year",(YEAR))
	private val monday = ("monday",(DOW(1)(todaysDate,0)))
	private val tuesday = ("tuesday",(DOW(2)(todaysDate,0)))
	private val wednesday = ("wednesday",(DOW(3)(todaysDate,0)))
	private val thursday = ("thursday",(DOW(4)(todaysDate,0)))
	private val friday = ("friday",(DOW(5)(todaysDate,0)))
	private val saturday = ("saturday",(DOW(6)(todaysDate,0)))
	private val sunday = ("sunday",(DOW(7)(todaysDate,0)))
	private val monday_neg1 = ("monday",(DOW(1)(todaysDate,-1)))
	private val tuesday_neg1 = ("tuesday",(DOW(2)(todaysDate,-1)))
	private val wednesday_neg1 = ("wednesday",(DOW(3)(todaysDate,-1)))
	private val thursday_neg1 = ("thursday",(DOW(4)(todaysDate,-1)))
	private val friday_neg1 = ("friday",(DOW(5)(todaysDate,-1)))
	private val saturday_neg1 = ("saturday",(DOW(6)(todaysDate,-1)))
	private val sunday_neg1 = ("sunday",(DOW(7)(todaysDate,-1)))
	private val special_chars = ("today '",(REF(todaysDate)))
	private val lasthalf1989 = ("last half 1989",(Range(Time(1989,7),Time(1990))))
	private val lastquarter1989 = ("last quarter 1989",(Range(Time(1989,10),Time(1990))))
	private val recentMonths = ("recent months",(PAST))
	//--Hard Real Data
	private val may22sp1995 
		= ("May 22 , 1995", (Range(Time(1995,5,22),Time(1995,5,23))))


	private case class ToyStore(gold:Array[(String,Temporal)],test:Boolean) 
			extends DataStore[(TimeSent,Temporal,Time)] {
		override def name:String = if(test) "toy-dev" else "toy"
		override def eachExample(iter:Int):Iterable[(TimeSent,Temporal,Time)] = {
			val score:Score = new Score
			gold.zipWithIndex.map{ case ((sent:String,gold:Temporal),id:Int) =>
				//(variables)
				val words = sent.split(" ").map{ (raw:String) => 
					val (str,typ) = 
						if(raw.length > 2 && raw.endsWith("st") &&
								(raw.substring(0,raw.length-2) matches G.CanInt)){
							(raw.substring(0,raw.length-2),NumberType.ORDINAL)
						} else if(raw matches G.CanInt){
							(raw,NumberType.NUMBER)
						} else {
							(raw, NumberType.NONE)
						}
					(index.str2wTest(str),typ,str)
				}
				val s = TimeSent(
					words.map{ _._1 }, 
					words.map{ x => index.str2posTest("UNK") },
					words.map{ case (w:Int,t:NumberType.Value,str:String) =>
						if(t != NumberType.NONE){ str.toInt } else { Int.MinValue }
					},
					words.map{ _._2 },
					index
					)
				(s,gold(todaysDate,0),todaysDate)
			}
		}
		def internWords:ToyStore = {
			gold.foreach{ case (sent:String,gold:Temporal) =>
				sent.split(" ").foreach{ (str:String) => 
					index.str2w(str)
				}
			}
			this
		}
	}

	def TODAY_ONLY:TimeData[(TimeSent,Temporal,Time)] = {
		TimeData(store(false,today).internWords,store(true,today))
	}
	
	def STANDARD:TimeData[(TimeSent,Temporal,Time)] = {
		TimeData(
			store(false,
			//--Train
				//(durations)
				aWeek,aMonth,aQuarter,ayear,weeks2,months2,weeksDash2,week2Period,year5,
					year5AndJunk,
				//(sequences)
				week,month,quarter,year,day,theWeek,
				//(cannonicals -> sequences)
				thisWeek,thisYear,thisMonth,
				//(shifts -- standard)
				lastWeek,lastYear,lastQuarter,nextMonth,weekLast,aWeekAgo,
				//(shifts -- noncannonical)
				pastWeek,thePastWeek,pastYear,pastMonths2,
				//(numbers -- basic)
				y1776,
				//(sequences)
				april,
				//(intersects)
				april1776,april2,
				//(days of the week)
				monday,tuesday,wednesday,thursday,friday,saturday,sunday,
				//(numbers -- complex)
				y17sp76,
				//(seasons)
				spring,summer,fall,winter,
				//(floor/ceil)
				quarter, firstQuarter, secondQuarter, thirdQuarter,fourthQuarter,

//				//(offset -1)
//				monday_neg1,tuesday_neg1,wednesday_neg1,thursday_neg1,friday_neg1,saturday_neg1,sunday_neg1,
//				//(hard)
//				lasthalf1989, lastquarter1989,recentMonths,
//				may22sp1995,special_chars,
				//(ref)
				today
			).internWords,
			//--Test
			store(true,lastMonth))
	}
}


