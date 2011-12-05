package time

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.ListMap
import scala.collection.mutable.HashMap

import java.util.IdentityHashMap

import org.joda.time._

import edu.stanford.nlp.stats.ClassicCounter
import edu.stanford.nlp.stats.Counter
import edu.stanford.nlp.stats.Counters
import edu.stanford.nlp.util.logging.Redwood.Util._

import org.apache.commons.math.distribution.NormalDistributionImpl

import Temporal.{TraverseFn,TraverseTask}

//------------------------------------------------------------------------------
// TEMPORAL
//------------------------------------------------------------------------------
trait Temporal {

	import Temporal.isInt
	//(get values)
	def evaluate[E <: Temporal](ground:GroundedRange,offset:Long):(TraverseFn,E)
	def prob(ground:GroundedRange,offset:Long):Double
	def exists(ground:GroundedRange,offset:Long):Boolean = { offset == 0 }
	//(learn distribution)
	def updateE(ground:GroundedRange,
		offset:Long,originoffset:Long,logprob:Double):Unit = {}
	def runM:Unit = {}
	
	final def traverse(ground:GroundedRange,offset:Long,	
			fn:TraverseTask):Unit = {
		val (feedback,rtn):(TraverseFn,Temporal) = evaluate(ground,offset)
		feedback(fn)
	}

	//(helper functions)
	final def forwardIterator[E <: Temporal](ground:GroundedRange
			):BufferedIterator[(TraverseFn,E)] = {
		var rightPointer:Long = 0;
		new Iterator[(TraverseFn,E)]{
			def hasNext:Boolean = Temporal.this.exists(ground,rightPointer)
			def next:(TraverseFn,E) = {
				val rtn = evaluate[E](ground,rightPointer)
				assert(!rtn.isInstanceOf[NoTime], 
					"NoTime in iterator (hasNext: "+hasNext+")")
				rightPointer += 1
				rtn
			}
		}.buffered
	}
	final def backwardIterator[E <: Temporal](ground:GroundedRange
			):BufferedIterator[(TraverseFn,E)]={
		var leftPointer:Long = 0; //NOTE: will both hit zero
		new Iterator[(TraverseFn,E)]{
			def hasNext:Boolean = Temporal.this.exists(ground,leftPointer)
			def next:(TraverseFn,E) = {
				val rtn = evaluate[E](ground,leftPointer)
				assert(!rtn.isInstanceOf[NoTime], 
					"NoTime in iterator (hasNext: "+hasNext+")")
				leftPointer -= 1
				rtn
			}
		}.buffered
	}
	final def forwardIterable[E <: Temporal](ground:GroundedRange
			):Iterable[(TraverseFn,E)] = {
		new Iterable[(TraverseFn,E)]{ 
			def iterator:Iterator[(TraverseFn,E)] = forwardIterator(ground)
		}
	}
	final def backwardIterable[E <: Temporal](ground:GroundedRange
			):Iterable[(TraverseFn,E)] = {
		new Iterable[(TraverseFn,E)]{ 
			def iterator:Iterator[(TraverseFn,E)] = backwardIterator(ground)
		}
	}
	final def distribution(ground:GroundedRange
			):Iterator[(Temporal,Double,Long)] = {
		var leftPointer = -1;
		var rightPointer = 0;
		new Iterator[(Temporal,Double,Long)]{
			def hasNext:Boolean
				= Temporal.this.exists(ground,leftPointer) || 
				  Temporal.this.exists(ground,rightPointer)
			def next:(Temporal,Double,Long) = {
				assert(hasNext, "Calling next when there is no next")
				val pLeft = prob(ground,leftPointer)
				val pRight = prob(ground,rightPointer)
				if(Temporal.this.exists(ground,leftPointer) && 
						(pLeft >= pRight || !Temporal.this.exists(ground,rightPointer))) {
					assert(Temporal.this.exists(ground,leftPointer),"invalid if cond")
					val rtn:Temporal = Temporal.this.evaluateTemporal(ground,leftPointer)
					assert(!rtn.isInstanceOf[NoTime], 
						"NoTime in distribution (hasNext: "+hasNext+"): " + 
							Temporal.this + " at " + leftPointer)
					leftPointer -= 1
					(rtn,pLeft,leftPointer+1)
				} else if(Temporal.this.exists(ground,rightPointer)){
					assert(Temporal.this.exists(ground,rightPointer),"invalid if cond")
					val rtn:Temporal = Temporal.this.evaluateTemporal(ground,rightPointer)
					assert(!rtn.isInstanceOf[NoTime], 
						"NoTime in distribution (hasNext: "+hasNext+"): " + 
							Temporal.this + " at " + rightPointer)
					rightPointer += 1
					(rtn,pRight,rightPointer-1)
				} else {
					assert(!hasNext, "Inconsistent iterator")
					throw new NoSuchElementException()
				}
			}
		}
	}
	
	final def apply[E <: Temporal](ground:GroundedRange,offset:Long):E = {
		val (feedback,rtn) = evaluate[E](ground,offset)
		rtn
	}
	final def apply[E <: Temporal](ground:GroundedRange):E = {
		//(get distribution)
		val iter = distribution(ground)
		//(get best grounding)
		val rtn = if(iter.hasNext) {
			val (temporal,score,offset) = iter.next
			temporal
		} else {
			new NoTime
		} 
		//(return best grounding)
		rtn match {
			case (e:E) => e
			case _ => throw new IllegalArgumentException("Runtime Type Error")
		}
	}
	final def apply[E <: Temporal](ground:Time):Temporal
		= apply(new GroundedRange(ground,ground))
	final def apply[E <: Temporal](ground:Time,offset:Long):Temporal
		= apply(new GroundedRange(ground,ground),offset)
	final def bestOffset(ground:GroundedRange):Long = {
		//(get distribution)
		val iter = distribution(ground)
		//(get best grounding)
		if(iter.hasNext) {
			val (temporal,score,offset) = iter.next
			offset
		} else {
			0
		} 
	}
	final def bestOffset(ground:Time):Long 
		= bestOffset(new GroundedRange(ground,ground))

	final def all(ground:GroundedRange):Array[Temporal] = {
		distribution(ground).map( _._1 ).toArray
	}
	final def evaluateTemporal[E <: Temporal](ground:GroundedRange,offset:Long):E 
		= evaluate(ground,offset)._2

	final def timex3Type(ground:GroundedRange):String = this match {
//		case (s:Sequence) => "SET"
		case (r:Range) =>
			def getType(r:Range):String = {
				r match {
					case (gr:GroundedRange) => 
						if(gr.norm < Lex.DAY){ "TIME" } else { "DATE" }
					case _ => 
						r(ground) match {
							case (gr:GroundedRange) => gr match {
								case (nt:NoTime) => "UNK"
								case _ => getType(gr)
							}
							case _ => 
								throw new IllegalArgumentException("Unk grounding: "+r(ground))
						}
				}
			}
			getType(r)
		case (d:Duration) => "DURATION"
		case (pt:PartialTime) => "DATE"
		case _ => "UNK"
	}

	final def timex3Value(ground:GroundedRange):String = {
		import Lex._
		import Temporal.{df2,df4}
		def getValue(r:Range):String = {
			r match {
				case (gr:GroundedRange) => 
					if(gr.begin == ground && gr.norm.seconds == 0){
						"PRESENT_REF"
					} else if((gr.begin eq Time.DAWN_OF) && (gr.end ne Time.END_OF)){
						"FUTURE_REF"
					} else if((gr.end eq Time.END_OF) && (gr.begin ne Time.DAWN_OF)){
						"PAST_REF"
					} else {
						val norm = gr.norm
						val year:String = df4.format(gr.begin.year)
						if( gr.begin.month == 1 && gr.begin.day == 1 && !gr.begin.hasTime &&
								gr.end.month == 1 && gr.end.day == 1 && !gr.end.hasTime ) {
							"" + gr.begin.year
						} else if( (gr.end.month-gr.begin.month) % 3 == 0 &&
							gr.begin.day == 1 && gr.end.day == 1 &&
							!gr.begin.hasTime && !gr.end.hasTime) {
							"" + year + "-Q" + gr.begin.quarter
						} else if( gr.begin.day == 1 && !gr.begin.hasTime &&
								gr.end.day == 1 && !gr.end.hasTime &&
								(gr.begin.year == gr.end.year ||
									(gr.begin.year+1 == gr.end.year && 
									 gr.begin.month == 12 && gr.end.month == 1) ) ) {
							"" + year + "-" + df2.format(gr.begin.month)
						} else if( isInt(norm / WEEK) ){
							"" + year + "-W" + df2.format(gr.begin.week)
						} else if( isInt(norm / DAY) ){
							"" + year + "-" + df2.format(gr.begin.month) + "-" + 
								df2.format(gr.begin.day)
						} else if( isInt(norm / HOUR) ){
							"" + year + "-" + df2.format(gr.begin.month) + "-" + 
								df2.format(gr.begin.day) + "T"+df2.format(gr.begin.hour)
						} else {
							gr.toString
						}
					}
				case _ => getValue(r(ground).asInstanceOf[GroundedRange])
			}
		}
		this match {
			case (r:Range) =>
				getValue(r)
			case (pt:PartialTime) => getValue(pt(ground).asInstanceOf[GroundedRange])
			case (d:Duration) => d.timexString
			case _ => "UNK"
		}
	}
}

object Temporal {
	//(term, true_offset, origin_offset)
	//  true_offset: the offset of the temporal
	//  origin_offset: the offset to calculate differences from
	//     i.e. 
	type TraverseTask = (Temporal,Long,Long)=>Unit
	type TraverseFn = TraverseTask=>Unit
	def fnCat( a:TraverseFn, b:TraverseFn ) = {
		(fn:TraverseTask)=> {
			a(fn)
			b(fn)
		}
	}
	def join[A](array:Array[A], str:String) = {
		if(array.length == 0){
			""
		} else {
			val sb:StringBuilder = new StringBuilder
			array.foreach( (a:A) => {
				sb.append(a).append(str)
			})
			sb.substring(0, sb.length - str.length)
		}
	}
	def isInt(d:Double):Boolean = {
		math.floor(d) == d
	}
	val df2:java.text.DecimalFormat = new java.text.DecimalFormat("00")
	val df4:java.text.DecimalFormat = new java.text.DecimalFormat("0000")

	var reader:scala.tools.nsc.interpreter.JLineReader = null
	var interpreter:scala.tools.nsc.interpreter.IMain = null
	def interactive = {
		import scala.tools.nsc.interpreter.{IMain,JLineReader,JLineCompletion}
		import scala.tools.nsc.Settings
		//--Create Interpreter
		println("Loading interpreter...")
		if(reader == null){
			//(objects)
			val settings = new Settings
			settings.usejavacp.value = true
			interpreter = new IMain(settings)
			//(initialize)
			interpreter.interpret("import time._")
			interpreter.interpret("import time.Lex._")
			interpreter.interpret("val ground = Time(2011,4,26)")
			interpreter.interpret(
				"org.joda.time.DateTimeZone.setDefault(org.joda.time.DateTimeZone.UTC);"
			)
			interpreter.interpret("""
				def f(t:Temporal) = {
					t.distribution(Range(ground,ground))
						.slice(0,10).foreach{ x => println(x) }
				}
			""")
			interpreter.interpret("""def today = Range(todaysDate,todaysDate)""")
			reader = new JLineReader(new JLineCompletion(interpreter))
		}
		//--Loop
		var cond = true
		while(cond){
			val str = reader.readLine("scala> ")
			interpreter.interpret(str)
		}
	}

}


//------------------------------------------------------------------------------
// RANGE
//------------------------------------------------------------------------------
// ----- RANGE -----
trait Range extends Temporal{
	def >>(diff:Duration):Range //shift right
	def <<(diff:Duration):Range //shift left
	def |>(diff:Duration):Range //extend right
	def <|(diff:Duration):Range //extend left
	def >|(diff:Duration):Range //shirnk to right
	def |<(diff:Duration):Range //shrink to left
	def !(durr:Duration):Range  //canonicalize
	def norm:GroundedDuration   //take the norm

	def <<!(dur:Duration):Range = (this << dur) ! dur
	def >>!(dur:Duration):Range = (this >> dur) ! dur

	def neverIntersects:List[Range=>Boolean] = List[Range=>Boolean]()

//	def cons(other:Range):Range = {
//		(this, other) match {
//			case (a:Range,b:NoTime) => new NoTime
//			case (a:GroundedRange,b:GroundedRange) => 
//				if(a.begin < b.end){ new GroundedRange(a.begin,b.end) }
//				else { new NoTime }
//			case _ => composite("cons",other,
//				Range.cons(_:Boolean,
//					_:Iterable[(TraverseFn,GroundedRange)],
//					_:Iterable[(TraverseFn,GroundedRange)]))
//		}
//	}

	def ^(other:Range):Range = {
		(this, other) match {
			case (a:Range,b:NoTime) => new NoTime
			case (a:GroundedRange,b:GroundedRange) => 
				val cand = new GroundedRange(
					Range.mkBegin(a.begin,b.begin),
					Range.mkEnd(a.end,b.end)
				)
				if(cand.end < cand.begin){ new NoTime }
				else{ cand }
			case (a:GroundedRange,b:RepeatedRange) => b.intersect(a) //shortcut
			case (a:RepeatedRange,b:GroundedRange) => a.intersect(b) //.
			case _ => Range.intersectSearch(this,other)
		}
	}

}

// ----- COMPOSITE RANGE -----
class CompositeRange( 
			applyFn:(GroundedRange,Long)=>(TraverseFn,GroundedRange),
			probFn:(GroundedRange,Long)=>Double,
			existsFn:(GroundedRange,Long)=>Boolean,
			theNorm:GroundedDuration,
			ops:List[String],
			moveOffset:Long
		) extends Sequence {
	
	override def evaluate[E <: Temporal](ground:GroundedRange,rawOffset:Long
			):(TraverseFn,E)={
		val offset = rawOffset + moveOffset
		assert(existsFn(ground,offset),"Applying when exists is not satisfied (ev)")
		val (tFn,rtn) = applyFn(ground,offset)
		assert(rtn.isInstanceOf[GroundedRange], "Composite ungrounded")
		rtn match {
			case (e:E) =>
				(Temporal.fnCat(tFn, (fn:TraverseTask) => fn(this,offset,0)), e)
			case _ => throw new IllegalArgumentException("Runtime Type Error")
		}
	}
	override def prob(ground:GroundedRange,rawOffset:Long):Double = {
		val offset = rawOffset //keep old prob
		super.prob(ground,offset) * probFn(ground,offset)
	}
	override def exists(ground:GroundedRange,rawOffset:Long):Boolean = {
		val offset = rawOffset + moveOffset //new exists though
		existsFn(ground,offset)
	}

	override def move(offset:Long)
		= new CompositeRange(applyFn,probFn,existsFn,theNorm,ops,moveOffset+offset)

	// -- Prune Intersection --
	private var neverIntersectsImpl = List[Range=>Boolean]()
	def prohibitIntersectWith(lst:List[Range=>Boolean]):CompositeRange = {
		neverIntersectsImpl = lst ::: neverIntersectsImpl
		this
	}
	override def neverIntersects:List[Range=>Boolean] = neverIntersectsImpl
	
	// -- Range Functions --
	override def +(diff:Duration):Duration 
		= extend((r:GroundedRange) => Range(r.begin,r.end + diff), norm+diff, "+")
	override def -(diff:Duration):Duration
		= extend((r:GroundedRange) => Range(r.begin,r.end - diff), norm-diff, "-")
	override def *(n:Long):Duration
		= extend((r:GroundedRange) => Range(r.begin,r.begin+r.norm*n),norm*n,"*"+n)
	//(TODO these should be factored out of Sequence)
	override def interval:GroundedDuration = norm
	override def seconds:Long = norm.seconds
	override def units:Array[DurationUnit.Value] = norm.units

	override def >>(diff:Duration):Range = extend( _ >> diff, norm, ">>" )
	override def <<(diff:Duration):Range = extend( _ << diff, norm, "<<" )
	override def |>(diff:Duration):Range = extend( _ |> diff, norm, "|>" )
	override def <|(diff:Duration):Range = extend( _ <| diff, norm, "<|" )
	override def >|(diff:Duration):Range = extend( _ >| diff, norm, ">|" )
	override def |<(diff:Duration):Range = extend( _ |< diff, norm, "|<" )
	override def !(diff:Duration):Range  = extend( _ !  diff, norm, "!" )
	override def norm:GroundedDuration = this.theNorm

	private def extend(fn:GroundedRange=>Range, newNorm:Duration, op:String) = {
		new CompositeRange( 
			(ground:GroundedRange,offset:Long) => { 
				val (tFn,rtn) = this.applyFn(ground,offset)
				(Temporal.fnCat(tFn, 
					(task:TraverseTask) => task(this,offset,0)),
				fn(rtn).asInstanceOf[GroundedRange]  )
			},
			(ground:GroundedRange,offset:Long) => this.probFn(ground,offset),
			(ground:GroundedRange,offset:Long) => this.existsFn(ground,offset),
			newNorm.interval,
			op :: this.ops,
			moveOffset
		)
	}
	
	override def equals(o:Any):Boolean = this eq o.asInstanceOf[AnyRef]
	override def toString:String = Temporal.join(ops.toArray, " <- ")
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}

// ----- GROUNDED RANGE -----
class GroundedRange(val begin:Time,val end:Time) extends Range {
	override def evaluate[E <: Temporal](ground:GroundedRange,offset:Long):(TraverseFn,E)={
		if(offset == 0){
			this match{ 
				case (e:E) => ( (fn:TraverseTask) => fn(this,offset,0), e )
				case _ => throw new IllegalArgumentException("Runtime Type Error")
			}
		} else {
			throw new TimeException("GroundedRange given nonzero offset: "+offset)
		}
	}
	override def prob(ground:GroundedRange,offset:Long):Double
	 = if(offset == 0){ 1.0 } else{ 0.0 }

	
	override def >>(diff:Duration):Range = new GroundedRange(begin+diff,end+diff)
	override def <<(diff:Duration):Range = new GroundedRange(begin-diff,end-diff)
	override def <|(diff:Duration):Range = new GroundedRange(begin-diff,begin)
	override def |>(diff:Duration):Range = new GroundedRange(end,end+diff)
	override def |<(diff:Duration):Range = new GroundedRange(begin,begin+diff)
	override def >|(diff:Duration):Range = new GroundedRange(end-diff,end)

	override def !(dur:Duration):Range = {
		import Lex.{AYEAR,QUARTER,MONTH,WEEK,DAY,HOUR,MIN,SEC}
		val base = begin.canonical(dur)
		val diff = dur.smallestUnit match {
			case DurationUnit.MILLENIUM => AYEAR*1000
			case DurationUnit.CENTURY => AYEAR*100
			case DurationUnit.DECADE => AYEAR*10
			case DurationUnit.YEAR => AYEAR
			case DurationUnit.QUARTER => QUARTER
			case DurationUnit.MONTH => MONTH
			case DurationUnit.WEEK => WEEK
			case DurationUnit.DAY => DAY
			case DurationUnit.HOUR => HOUR
			case DurationUnit.MINUTE => MIN
			case DurationUnit.SECOND => SEC
			case DurationUnit.ZERO => Duration.ZERO
		}
		new GroundedRange(base,base+diff)
	}

	
	def norm:GroundedDuration = (end - begin).interval

	override def equals(o:Any):Boolean = o match {
		case (gr:GroundedRange) => 
			gr.begin.equals(this.begin) && gr.end.equals(this.end)
		case _ => false
	}
	override def toString:String = "["+begin+", "+end+")"
	override def hashCode:Int = begin.hashCode ^ end.hashCode
}


// ----- UNGROUNDED RANGE -----
class UngroundedRange(val normVal:Duration,val beginOffset:Duration
		) extends Range{
	//(sanity checks)
	assert(beginOffset < Duration.INFINITE && beginOffset > Duration.NEG_INFINITE,
		"Ungrounded range with infinite offset")

	override def evaluate[E <: Temporal](ground:GroundedRange,offset:Long):(TraverseFn,E)={
		if(offset == 0){ 
			val gr:GroundedRange = 
				if(normVal eq Duration.NEG_INFINITE){
					new GroundedRange(Time.DAWN_OF, ground.begin+beginOffset)
				} else if(normVal eq Duration.INFINITE){
					new GroundedRange(ground.begin+beginOffset,Time.END_OF)
				} else if(normVal.seconds == 0){
					new GroundedRange(ground.begin+beginOffset,
						ground.end+beginOffset)
				} else {
					new GroundedRange(ground.begin+beginOffset,
						ground.begin+beginOffset+normVal)
				}
			gr match {
				case (e:E) => ( (fn:TraverseTask) => fn(this,offset,0), e )
				case _ => throw new IllegalArgumentException("Runtime Type Error")
			}
		} else{
			throw new TimeException("UngroundedRange given nonzero offset: "+offset)
		}
	}
	override def prob(ground:GroundedRange,offset:Long):Double
		= if(offset == 0){ 1.0 } else{ 0.0 }

	override def >>(diff:Duration):Range =
		if(diff == Duration.INFINITE){ Range(Time.END_OF, Time.END_OF) }
		else if(diff == Duration.NEG_INFINITE){ Range(Time.DAWN_OF, Time.DAWN_OF) }
		else { new UngroundedRange(normVal,beginOffset+diff) }

	override def <<(diff:Duration):Range =
		if(diff == Duration.INFINITE){ Range(Time.DAWN_OF, Time.DAWN_OF) }
		else if(diff == Duration.NEG_INFINITE){ Range(Time.END_OF, Time.END_OF) }
		else { new UngroundedRange(normVal,beginOffset-diff) }

	override def <|(diff:Duration):Range
		= if(diff.isInstanceOf[FuzzyDuration]){
			Lex.PAST
		} else {
			if(diff == Duration.INFINITE){ Lex.PAST }
			else if(diff == Duration.NEG_INFINITE){ Lex.FUTURE }
			new UngroundedRange(diff,beginOffset-diff)
		}

	override def |>(diff:Duration):Range 
		= if(diff.isInstanceOf[FuzzyDuration]){
			Lex.FUTURE
		} else {
			if(normVal == Duration.INFINITE){ Range(Time.END_OF,Time.END_OF) }
			else if(normVal == Duration.NEG_INFINITE){ 
				Range(Time.DAWN_OF,Time.DAWN_OF) }
			else if(diff == Duration.INFINITE){ Lex.FUTURE }
			else if(diff == Duration.NEG_INFINITE){ Lex.PAST }
			else { new UngroundedRange(diff,beginOffset+normVal) }
		}

	override def |<(diff:Duration):Range = {
		if(diff < 0){ new NoTime }
		else if(diff == Duration.INFINITE){ Range(Time.END_OF, Time.END_OF) }
		else { new UngroundedRange(diff,beginOffset) }
	}

	override def >|(diff:Duration):Range = {
		assert(diff >= 0, "Shrinking to a negative duration")
		if(diff == Duration.INFINITE){ Range(Time.DAWN_OF, Time.DAWN_OF) }
		else { new UngroundedRange(diff,beginOffset+normVal-diff) }
	}
	
	def norm:GroundedDuration = normVal.interval
	
	override def !(dur:Duration):Range = {
		new CompositeRange(
				(ground:GroundedRange,offset:Long) => {
					val (tFn,grounded):(TraverseFn,GroundedRange) 
						= this.evaluate(ground,offset)
					grounded match {
						case (gr:GroundedRange) =>
							(Temporal.fnCat(tFn,(fn:TraverseTask) => fn(this,offset,0)),
							 (gr ! dur).asInstanceOf[GroundedRange])
						case _ => throw new IllegalArgumentException("Runtime Type Error")
					}
				},
				this.prob(_:GroundedRange,_:Long),
				this.exists(_:GroundedRange,_:Long),
				dur.interval,
				List[String](this + " ! " + dur),
				0L
			)
	}
	
	override def equals(o:Any):Boolean = o match {
		case (ur:UngroundedRange) => 
			ur.normVal.equals(this.normVal) && ur.beginOffset.equals(this.beginOffset)
		case _ => false
	}
	override def toString:String 
		= "[x"+{if(beginOffset.seconds==0) "" else "+"+beginOffset}+", x+"+
			(beginOffset+normVal)+")"
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}



// ----- OBJECT RANGE -----
object Range {
	def apply(begin:Time,end:Time) = new GroundedRange(begin,end)
	def apply(begin:Time) = new GroundedRange(begin,begin)
	def apply(begin:DateTime,end:DateTime) 
		= new GroundedRange(Time(begin),Time(end))
	def apply(begin:DateTime) 
		= new GroundedRange(Time(begin),Time(begin))
	def apply(begin:GroundedRange,end:GroundedRange):Range = apply(
			{if(begin.begin < end.begin) begin.begin else end.begin},
			{if(begin.end < end.end) begin.end else end.end})

	def apply(norm:Duration) = new UngroundedRange(norm,Duration.ZERO)
	def apply(norm:Period) = new UngroundedRange(Duration(norm),Duration.ZERO)

	def mkBegin(a:Time,b:Time) = if(a < b) b else a
	def mkEnd(a:Time,b:Time) = if(a < b) a else b

	case class OverlapState(origin:Long,offset:Long,
			nextVal:(TraverseFn,GroundedRange),
			iter:BufferedIterator[(TraverseFn,GroundedRange)]) {
		def this(iter:BufferedIterator[(TraverseFn,GroundedRange)])
			= this(0,0,iter.next,iter)
		def nextRange:GroundedRange = {
			assert(nextVal != null, "Taking value of empty overlap state")
			nextVal._2
		}
		def increment:OverlapState = {
			if(iter.hasNext){
				new OverlapState(origin,offset+1,iter.next,iter)
			} else {
				new OverlapState(origin,offset+1,null,iter)
			}
		}
		def markOrigin:OverlapState = new OverlapState(offset,offset,nextVal,iter)
	}
		
	def mknext2iterable( 
			a:Iterable[(TraverseFn,GroundedRange)], 
			b:Iterable[(TraverseFn,GroundedRange)],
			mkNext:(OverlapState,OverlapState)=>
				((TraverseFn,GroundedRange),OverlapState,OverlapState)
			):Iterable[((TraverseFn,GroundedRange),Long,Long)] = {
		
		new Iterable[((TraverseFn,GroundedRange),Long,Long)]{
			def iterator:Iterator[((TraverseFn,GroundedRange),Long,Long)] = {
				new Iterator[((TraverseFn,GroundedRange),Long,Long)]{
					private val iterA = a.iterator.buffered
					private val iterB = b.iterator.buffered
					private var (theNext,leftState,rightState)
						:((TraverseFn,GroundedRange),OverlapState,OverlapState)
						= if(iterA.hasNext && iterB.hasNext){
								mkNext(new OverlapState(iterA), new OverlapState(iterB))
							} else {
								(null,null,null)
							}
					override def hasNext:Boolean 
						= theNext != null
					override def next:((TraverseFn,GroundedRange),Long,Long) = {
						if(theNext == null){ throw new NoSuchElementException }
						val rtn=(theNext,leftState.offset,rightState.offset)
						val (n,lS,rS) = mkNext(leftState,rightState)
						theNext = n; leftState = lS; rightState = rS;
						rtn
					}
				}
			}
		}
		
	}

//	def cons(back:Boolean,
//			a:Iterable[(TraverseFn,GroundedRange)],
//			b:Iterable[(TraverseFn,GroundedRange)]
//			):Iterable[((TraverseFn,GroundedRange),Int,Int)] = {
//		var diff:Duration = Duration.INFINITE
//		def mkNext(left:OverlapState,right:OverlapState
//				):((TraverseFn,GroundedRange),OverlapState,OverlapState) = {
//			val nullVal = (null,null,null)
//			if(left.nextVal==null || right.nextVal==null){
//				//(case: an iterator is empty)
//				nullVal
//			} else if(right.nextRange.end < left.nextRange.begin){
//				//(case: B is behind)
//				//((convergence check))
//				val lastDiff = diff
//				diff = (left.nextRange.begin-right.nextRange.end)
//				if(!(diff < lastDiff)){ nullVal } //case: not converging
//				//((movement))
//				else if(!back && right.iter.hasNext)
//					{ mkNext(left,right.increment.markOrigin) }
//				else if(back && left.iter.hasNext)
//					{ mkNext(left.increment.markOrigin,right) }
//				else { nullVal }
//			} else {
//				//(case: overlap)
//				val (fnLeft,rtnLeft) = left.nextVal
//				val (fnRight,rtnRight) = right.nextVal
//				val rtn = (
//							(fn:TraverseTask) => { 
//								fnLeft( (term:Temporal,offset:Long,orig:Int) => {
//									fn(term,offset,if(back){-left.origin}else{left.origin}) })
//								fnRight( (term:Temporal,offset:Long,orig:Int) => {
//									fn(term,offset,if(back){-right.origin}else{right.origin}) })
//							},
//							new GroundedRange(rtnLeft.begin,rtnRight.end)
//						)
//				//(update iterator)
//				(rtn,left.increment,right)
//			}
//		}
//		mknext2iterable(a,b,mkNext(_,_))
//	}
		class RangePair(val rA:Range,val rB:Range) {
			override def hashCode:Int = 
				System.identityHashCode(rA) ^ System.identityHashCode(rB)
			override def equals(o:Any) = o match {
				case (other:RangePair) =>
					(rA.toString == other.rA.toString) && 
					(rB.toString == other.rB.toString)
				case _ => false
			}
		}
		val intersectCache = new HashMap[RangePair,Range]()
		def intersectSearch(rA:Range,rB:Range):Range = {
			//--Cache and Pruning
			//(cache)
			if(intersectCache.contains(new RangePair(rA,rB))){
				return intersectCache(new RangePair(rA,rB))
			}
			//(pruning)
			if( !rA.neverIntersects.forall{ (f:Range=>Boolean) => !f(rB) } ||
			    !rB.neverIntersects.forall{ (f:Range=>Boolean) => !f(rA) } ){
				return new NoTime
			}
			log(FORCE,"Create search for " + rA + " ^ " + rB + " " + System.identityHashCode(rA) + ","+System.identityHashCode(rB))
			//--Classes
			case class RangeTerm(r:GroundedRange,ground:GroundedRange
					) extends Intersectable{
				override def begin:Long 
					= r.begin.base.getMillis-ground.begin.base.getMillis
				override def end:Long 
					= r.end.base.getMillis-ground.begin.base.getMillis
				override def toString:String = r.toString+" {"+begin+","+end+"}"
			}
			case class RangeSource(r:Range,ground:GroundedRange) 
					extends ProvidesIntersectables[RangeTerm]{
				override def has(offset:Long):Boolean = {
					r.exists(ground,offset)
				}
				override def intersectable(offset:Long):RangeTerm = {
					val (fn,rng):(TraverseFn,GroundedRange) 
						= r.evaluate(ground,offset)
					RangeTerm(rng,ground)
				}
			}
			val noTerm:(TraverseFn,GroundedRange,Double,Boolean) = 
					((fn:TraverseTask) => (term:Temporal,offset:Long,orig:Long)=>{},
					  new NoTime,
					  0.0,
						false
					)
			//--Search State
			val stateMap=new HashMap[GroundedRange,IteratorMap[Intersection]]
			//--Get Info
			val info:(GroundedRange,Long)=>(TraverseFn,GroundedRange,Double,Boolean) =
			(ground:GroundedRange,offset:Long) => {
				assert(offset < Int.MaxValue && offset > Int.MinValue,
					"Wildly innapropriate offset. Don't do that.")
				//(get map)
				val map = if(stateMap.contains( ground )){
					stateMap( ground )
				} else {
					//(intersect)
					val sourceA = RangeSource(rA,ground)
					val sourceB = RangeSource(rB,ground)
					val iter = new IteratorMap( 
						Intersect.intersectForward(sourceA,sourceB),
						Intersect.intersectBackward(sourceA,sourceB)) //<--intersect here!
					//(save)
					stateMap( ground ) = iter
					iter
				}
				//(early exit)
				if(!map.contains(offset.toInt)) { //checked by assert above
					noTerm
				} else {
					//(create info)
					val intersect = map.get(offset.toInt)
					intersect match {
					case Some(intersect) =>
						val (originA,originB) = intersect.origin
						if(rA.exists(ground,intersect.a) && rB.exists(ground,intersect.b)){
							//^TODO this check really shouldn't have to be here
							//((term A))
							val (fnA,grA):(TraverseFn,GroundedRange) =
								rA.evaluate(ground,intersect.a)
							val (fnOriginA,grOriginA):(TraverseFn,GroundedRange) =
								rA.evaluate(ground,originA)
							val probA = rA.prob(grOriginA,intersect.a-originA)
							//((term B))
							val (fnB,grB):(TraverseFn,GroundedRange) = 
								rB.evaluate(ground,intersect.b)
							val (fnOriginB,grOriginB):(TraverseFn,GroundedRange) =
								rB.evaluate(ground,originB)
							val probB = rB.prob(ground,intersect.b-originB)
							//(return)
							assert(probA >= 0 && probA <= 1, "Not a probability: " + probA)
							assert(probB >= 0 && probB <= 1, "Not a probability: " + probB)
							( (fn:TraverseTask) => {
									fnA( (term:Temporal,offset:Long,orig:Long) => {
										fn(grA,intersect.a,originA) })
									fnB( (term:Temporal,offset:Long,orig:Long) => {
										fn(grB,intersect.b,originB) })
									
								}, //<-- traverse function
								new GroundedRange(
									Range.mkBegin(grA.begin,grB.begin),
									Range.mkEnd(grA.end,grB.end)
								), //<-- temporal
								probA*probB, //<-- probability
								true) //<-- exists
						} else {
							noTerm
						}
					case None => noTerm
					}
				}
			}
			//--Create Functions
			//(probability)
			val probFn:(GroundedRange,Long)=>Double = 
				(g:GroundedRange,offset:Long) => {
					val (fn,gr,prob,exists) = info(g,offset)
					prob
				}
			//(exists)
			val existsFn:(GroundedRange,Long)=>Boolean = 
				(g:GroundedRange,offset:Long) => {
					val (fn,gr,prob,exists) = info(g,offset)
					exists && !gr.isInstanceOf[NoTime]
				}
			//(apply)
			val applyFn:(GroundedRange,Long)=>(TraverseFn,GroundedRange) =
				(g:GroundedRange,offset:Long) => {
					val (fn,gr,prob,exists) = info(g,offset)
					assert(existsFn(g,offset),"Applying when exists is not satisfied")
					(fn,gr)
				}
			//--Create Misc
			//(norm)
			val normA = rA.norm
			val normB = rA.norm
			val norm = if(normA < normB) normA else normB
			//(string)
			val ops = List[String]("("+rA+") ^ ("+rB+")")
			//--Return
			val rtn = new CompositeRange(applyFn,probFn,existsFn,norm,ops,0L)
				.prohibitIntersectWith(rA.neverIntersects)
				.prohibitIntersectWith(rB.neverIntersects)
			intersectCache(new RangePair(rA,rB)) = rtn
			return rtn;
		}
}

//------------------------------------------------------------------------------
// DURATION
//------------------------------------------------------------------------------
// ----- DURATION -----
trait Duration extends Temporal {
	override def evaluate[E <: Temporal](ground:GroundedRange,offset:Long):(TraverseFn,E)={
		if(offset == 0){
			this match {
				case (e:E) => 
					assert(e.isInstanceOf[GroundedDuration] || 
						e.isInstanceOf[FuzzyDuration], 
						"Duration not grounded")
					( (fn:TraverseTask) => fn(this,offset,0), e )
				case _ => throw new IllegalArgumentException("Runtime Type Error")
			}
		}
		else{ throw new TimeException("Duration given nonzero offset: "+offset) }
	}
	override def prob(ground:GroundedRange,offset:Long):Double
		= if(offset == 0){ 1.0 } else{ 0.0 }

	def interval:GroundedDuration
	def seconds:Long

	def +(diff:Duration):Duration 
	def -(diff:Duration):Duration
	def *(n:Long):Duration
	
	def /(other:Duration):Double 
		= this.seconds.asInstanceOf[Double] / other.seconds.asInstanceOf[Double]
	def <(other:Duration) = this.seconds < other.seconds
	def >(other:Duration) = this.seconds > other.seconds
	def <=(other:Duration) = this.seconds <= other.seconds
	def >=(other:Duration) = this.seconds >= other.seconds
	def <(other:Int) = this.seconds < other
	def >(other:Int) = this.seconds > other
	def <=(other:Int) = this.seconds <= other
	def >=(other:Int) = this.seconds >= other

	def units:Array[DurationUnit.Value]
	def smallestUnit:DurationUnit.Value = {
		val arr:Array[DurationUnit.Value] = units
		if(arr.length == 0){ DurationUnit.ZERO } else { arr(arr.length-1) }
	}
	def largestUnit:DurationUnit.Value = {
		val arr:Array[DurationUnit.Value] = units
		if(arr.length == 0){ DurationUnit.ZERO } else { arr(0) }
	}
	def unary_~ : Duration = new FuzzyDuration(this)

	def timexString:String = this.toString
	override def equals(o:Any):Boolean = o match{
		case (s:Sequence) => false
		case (d:Duration) => this.seconds == d.seconds
		case _ => false
	}
}

// ----- GROUNDED DURATION -----
class GroundedDuration(val base:ReadablePeriod) extends Duration {
	override def interval:GroundedDuration = this
	override def seconds:Long = {
		var period = base.toPeriod
		val monthContrib:Long = period.getMonths*30*24*60*60
		val yearContrib:Long = period.getYears.longValue*365*24*60*60
		period = period.withMonths(0).withYears(0)
		period.toStandardDuration.getStandardSeconds+monthContrib+yearContrib
	}

	override def +(diff:Duration):Duration
		= if(this.equals(Duration.INFINITE)){ Duration.INFINITE }
		  else if(this.equals(Duration.NEG_INFINITE)){ Duration.NEG_INFINITE }
		  else if(diff.equals(Duration.INFINITE)){ Duration.INFINITE }
		  else if(diff.equals(Duration.NEG_INFINITE)){ Duration.NEG_INFINITE }
		  else { 
				assert( !(this.equals(Duration.INFINITE) || 
					this.equals(Duration.NEG_INFINITE)))
				assert( !(diff.equals(Duration.INFINITE) || 
					diff.equals(Duration.NEG_INFINITE)))
				new GroundedDuration(base.toPeriod.plus(diff.interval.base))
			}
	override def -(diff:Duration):Duration
		= if(this.equals(Duration.INFINITE)){ Duration.INFINITE }
		  else if(this.equals(Duration.NEG_INFINITE)){ Duration.NEG_INFINITE }
		  else if(diff.equals(Duration.INFINITE)){ Duration.NEG_INFINITE }
		  else if(diff.equals(Duration.NEG_INFINITE)){ Duration.INFINITE }
		  else { 
				assert( !(this.equals(Duration.INFINITE) || 
					this.equals(Duration.NEG_INFINITE)))
				assert( !(diff.equals(Duration.INFINITE) || 
					diff.equals(Duration.NEG_INFINITE)))
				new GroundedDuration(base.toPeriod.minus(diff.interval.base))
			}
	override def *(n:Long):Duration = {
		import DurationUnit._
		try{
			def check(v:Long) = {
				assert(v <= Int.MaxValue && v >= Int.MinValue, "Bad multiply: " + v)
			}
			def addYears(p:Period,v:Long):Period  ={ check(v); p.plusYears(v.toInt)  }
			def addMonths(p:Period,v:Long):Period ={ check(v); p.plusMonths(v.toInt) }
			def addWeeks(p:Period,v:Long):Period  ={ check(v); p.plusWeeks(v.toInt)  }
			def addDays(p:Period,v:Long):Period   ={ check(v); p.plusDays(v.toInt)   }
			def addHours(p:Period,v:Long):Period  ={ check(v); p.plusHours(v.toInt)  }
			def addMinutes(p:Period,v:Long):Period={ check(v); p.plusMinutes(v.toInt)}
			def addSeconds(p:Period,v:Long):Period={ check(v); p.plusSeconds(v.toInt)}
			var p = base.toPeriod
			new GroundedDuration( units.foldLeft(Period.ZERO){ 
				case (soFar:Period,term:DurationUnit.Value) => term match {
					case MILLENIUM => 
						val v = (p.getYears/1000).toLong*n
						addYears(soFar,v)
					case CENTURY => 
						val v = (p.getYears%1000).toLong*n
						addYears(soFar,v)
					case DECADE => 
						val v = (p.getYears%100).toLong*n
						addYears(soFar,v)
					case YEAR => 
						val v = (p.getYears%10).toLong*n
						addYears(soFar,v)
					case QUARTER => 
						val v = p.getMonths.toLong*n
						addMonths(soFar,v)
					case MONTH => 
						val v = p.getMonths.toLong*n
						addMonths(soFar,v)
					case WEEK => 
						val v = p.getWeeks.toLong*n
						addWeeks(soFar,v)
					case DAY => 
						val v = p.getDays.toLong*n
						addDays(soFar,v)
					case HOUR => 
						val v = p.getHours.toLong*n
						if(v < Int.MinValue || v > Int.MaxValue){
							addHours(
								addDays(
									soFar,
									v/(24)
								),
								v % (24)
							)
						} else {
							addHours(soFar,v)
						}
					case MINUTE => 
						val v = p.getMinutes.toLong*n
						if(v < Int.MinValue || v > Int.MaxValue){
							addMinutes(
								addHours(
									addDays(
										soFar,
										v/(60*24)
									),
									v/(60) % (24)
								),
								v % (60)
							)
						} else {
							addMinutes(soFar,v)
						}
					case SECOND => 
						val v = p.getSeconds.toLong*n
						if(v < Int.MinValue || v > Int.MaxValue){
							addSeconds(
								addMinutes(
									addHours(
										addDays(
											soFar,
											v/(60*60*24)
										),
										v/(60*60) % (24)
									),
									v/60 % (60)
								),
								v % (60)
							)
						} else {
							addSeconds(soFar,v)
						}
				}
			} )
		} catch {
			case (e:ArithmeticException) =>
				if(n > 0){ Duration.INFINITE }
				else { Duration.NEG_INFINITE }
		}
	}

	override def units:Array[DurationUnit.Value] = {
		val period = base.toPeriod
		var unitsAsc = List[DurationUnit.Value]()
		if(period.getYears >= 1000){ unitsAsc = DurationUnit.MILLENIUM :: unitsAsc }
		if(period.getYears%1000 >= 100){unitsAsc = DurationUnit.CENTURY :: unitsAsc}
		if(period.getYears%100 >= 10){unitsAsc = DurationUnit.DECADE :: unitsAsc}
		if(period.getYears%10 > 0){unitsAsc = DurationUnit.YEAR :: unitsAsc}
		if(period.getMonths > 0 && period.getMonths % 3 == 0)
			{unitsAsc = DurationUnit.QUARTER :: unitsAsc}
		if(period.getMonths % 3 > 0){unitsAsc = DurationUnit.MONTH :: unitsAsc}
		if(period.getWeeks > 0){unitsAsc = DurationUnit.WEEK :: unitsAsc}
		if(period.getDays > 0){unitsAsc = DurationUnit.DAY :: unitsAsc}
		if(period.getHours > 0){unitsAsc = DurationUnit.HOUR :: unitsAsc}
		if(period.getMinutes > 0){unitsAsc = DurationUnit.MINUTE :: unitsAsc}
		if(period.getSeconds > 0){unitsAsc = DurationUnit.SECOND :: unitsAsc}
		unitsAsc.reverse.toArray
	}
	override def timexString:String = {
		import DurationUnit._
		def f(str:String,isTime:Boolean) = {
			if(!isTime) "T" + str else str
		}
		"P"+units.foldLeft(("",false)){ 
			case ((soFar:String,t:Boolean),unit:DurationUnit.Value) => 
				unit match {
					case MILLENIUM => 
						(soFar+(base.toPeriod.getYears/1000) + "L",false)
					case CENTURY   => 
						(soFar+((base.toPeriod.getYears%1000)/100) + "C",false)
					case DECADE    => 
						(soFar+((base.toPeriod.getYears%100)/10) + "E",false)
					case YEAR      => 
						(soFar+(base.toPeriod.getYears%10)+"Y",false)
					case QUARTER   => 
						(soFar+base.toPeriod.getMonths + "M",false)
					case MONTH     => 
						(soFar+base.toPeriod.getMonths + "M",false)
					case WEEK      => 
						(soFar+base.toPeriod.getWeeks + "W",false)
					case DAY       => 
						(soFar+base.toPeriod.getDays + "D",false)
					case HOUR      => 
						(soFar+f("" + base.toPeriod.getHours + "H",t),true)
					case MINUTE    => 
						(soFar+f("" + base.toPeriod.getMinutes + "M",t),true)
					case SECOND    => 
						(soFar+f("" + base.toPeriod.getSeconds + "S",t),true)
			}
		}._1
	}
	
	override def toString:String = this.base.toString
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}

class FuzzyDuration(val base:Duration) extends Duration {
	override def interval:GroundedDuration = base.interval
	override def seconds:Long = base.seconds

	override def +(diff:Duration):Duration = new FuzzyDuration( this.base + diff )
	override def -(diff:Duration):Duration = new FuzzyDuration( this.base - diff )
	override def *(n:Long):Duration        = new FuzzyDuration( this.base * n )

	override def units:Array[DurationUnit.Value] 
		= Array[DurationUnit.Value](base.largestUnit)
	override def unary_~ : Duration = this
	
	override def timexString:String 
		= Duration.cannonical(this.largestUnit).toString.replaceAll("[0-9]+","X")
	override def toString:String = "~"+this.base.toString
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}

// ----- OBJECT DURATION -----
object Duration {
	def apply(p:ReadablePeriod):GroundedDuration = new GroundedDuration(p)
	def apply(millis:Long):GroundedDuration = apply(new Period(millis))
	def cannonical(unit:DurationUnit.Value):GroundedDuration = {
		import DurationUnit._
		unit match {
			case MILLENIUM => this.apply(Years.years(1000))
			case CENTURY   => this.apply(Years.years(100))
			case DECADE    => this.apply(Years.years(10))
			case YEAR      => this.apply(Years.ONE)
			case QUARTER   => this.apply(Months.THREE)
			case MONTH     => this.apply(Months.ONE)
			case WEEK      => this.apply(Weeks.ONE)
			case DAY       => this.apply(Days.ONE)
			case HOUR      => this.apply(Hours.ONE)
			case MINUTE    => this.apply(Minutes.ONE)
			case SECOND    => this.apply(Seconds.ONE)
		}
	}
	val INFINITE:Duration = new GroundedDuration( Period.years(Int.MaxValue) )
	val NEG_INFINITE:Duration = new GroundedDuration( Period.years(Int.MinValue) )
	val ZERO:Duration = new GroundedDuration( new Period(0L) )
}

//------------------------------------------------------------------------------
// SEQUENCE
//------------------------------------------------------------------------------
// ----- SEQUENCE -----
trait Sequence extends Range with Duration {
	import Sequence.{Updater,Distribution}
	override def exists(ground:GroundedRange,offset:Long) = true
	override def equals(o:Any):Boolean = this eq o.asInstanceOf[AnyRef]

	// -- NECESSARY OVERRIDES --
	def move(offset:Long):Sequence

	// -- RECOMMENDED OVERRIDES --
	def diff(ground:GroundedRange,offset:Long,originOffset:Long):Double = {
		//(get points)
		val origin:GroundedRange = this.evaluateTemporal(ground,0)
		val virtualOrigin:GroundedRange = this.evaluateTemporal(ground,originOffset)
		val location:GroundedRange = this.evaluateTemporal(ground,offset)
		//(distance)
		val distance
			= (origin.begin-ground.begin)+(location.begin-virtualOrigin.begin)
		//(return)
		distance/norm
	}
	def isSparse:Boolean = true
	
	
	// -- EM --
	protected lazy val updater:Updater = {
		//(overhead)
		import O.Distribution._
		import O.Scope._
		import Sequence.{pointUpdater,multinomialUpdater,gaussianUpdater}
		import Sequence.{mkMultinomialUpdater,mkGaussianUpdater}
		//(routing)
		O.timeDistribution match {
			case Point => pointUpdater
			case Multinomial =>
				O.timeDistributionScope match {
					case Global => multinomialUpdater
					case Local =>if(isSparse) multinomialUpdater else mkMultinomialUpdater
				}
			case Gaussian =>
				O.timeDistributionScope match {
					case Global => gaussianUpdater
					case Local => if(isSparse) gaussianUpdater else mkGaussianUpdater
				}
		}
	}
	protected var distrib:Distribution = null


	override def updateE(
			ground:GroundedRange,offset:Long,originOffset:Long,logprob:Double):Unit={
		val (e,m) = updater
		val diff:Double = this.diff(ground,offset,originOffset)
		val str="E-Step [" + this + "]: offset=["+offset+" origin "+originOffset+
			"] diff="+G.df.format(diff)+" prob="+G.df.format(math.exp(logprob))+")"
		if(O.printAllParses){ log(FORCE,str) } else { log(FORCE,str) }
		assert(!logprob.isNaN, "NaN probability")
		e( offset-originOffset, diff, logprob )
	}

	override def runM:Unit = {
		val (e,m) = updater
		this.distrib = m(if(isSparse){"general"}else{this.toString},true)
	}
}

// ----- REPEATED RANGE -----
case class RepeatedRange(snapFn:Time=>Time,base:UngroundedRange,
		interv:Duration, bound:GroundedRange,moveOffset:Long) extends Sequence {

	//isSparse is true if this range is rare and should use the global stats
	private var isSparseVal = true
	override def isSparse:Boolean = isSparseVal
	def dense:RepeatedRange = { this.isSparseVal = false; this }

//	def this(snapFn:Time=>Time,base:UngroundedRange,interv:Duration) 
//		= this(snapFn,base,interv,null,0L)
	def this(snapFn:Time=>Time,base:Range,interv:Duration)
		= this(
			snapFn,
			base match {
				case (b:UngroundedRange) => b
				case _ => throw new IllegalArgumentException("Runtime Type Error")
			},
			interv,
			null,
			0L)

	override def neverIntersects:List[Range=>Boolean] = List[Range=>Boolean](
			(cand:Range) => cand match {
				case (rr:RepeatedRange) => 
					!(rr eq this) && //TODO somewhat hacky (relies on strict equality)
					rr.interv == this.interv &&
					rr.base == this.base
				case _ => false
			}
		) 
	
	override def diff(ground:GroundedRange,offset:Long,originOffset:Long):Double={
		val realGround:Time = if(bound == null) ground.begin else bound.begin
		//(important markers)
		val origin:GroundedRange = this.evaluateTemporal(ground,0)
		val virtualOrigin:GroundedRange = this.evaluateTemporal(ground,originOffset)
		val location:GroundedRange = this.evaluateTemporal(ground,offset)
		//(distance)
		val distance=(origin.begin-realGround)+(location.begin-virtualOrigin.begin)
		assert(interv.seconds > 0.0, "Interval is zero or negative: " + interv)
		assert(!(distance/interv).isNaN, 
			"NaN diff: " + origin + ", " + virtualOrigin + ", " + location + ": " + 
			realGround)
		distance/interv
	}

	override def move(offset:Long):Sequence = {
		new RepeatedRange(snapFn,base,interv,bound,moveOffset + offset).name(name)
	}
	
	override def evaluate[E <: Temporal](ground:GroundedRange,rawOffset:Long
			):(TraverseFn,E)={
		val offset = rawOffset + moveOffset
		var cache:Temporal = null; var cacheCond:Range = null
		val term = if( cache == null || ground != cacheCond) {
				//(update cache condition)
				cacheCond = ground
				//(get start)
				val beginT:Time = 
					if(bound == null) {
						ground.begin
					} else {
						if(ground.begin > bound.begin && ground.begin < bound.end)
							{ ground.begin }
						else { bound.begin }
					}
				//(snap beginning)
				val begin:Time = 
					if(bound == null){ snapFn(beginT+interv*offset) } 
					else{ snapFn(beginT+interv*offset) }
				//(ground the time)
				val rtn = new GroundedRange(
					begin+base.beginOffset,
					begin+base.beginOffset+base.norm)
				if(O.cacheTemporalComputations){ 
					cache = rtn
				} else{
					cache = null
					cacheCond = null
				}
				rtn
			} else {
				cache
			}
		//(return cache)
		val rtn:E = term match {
			case (e:E) => 
				assert(e.isInstanceOf[GroundedRange], "Range not grounded")
				e
			case _ => throw new IllegalArgumentException("Runtime Type Error")
		}
		( (fn:TraverseTask) => fn(this,offset,0), rtn )
	}
	
	override def exists(ground:GroundedRange,rawOffset:Long):Boolean = {
		val offset = rawOffset //evaluate already incorporates new offset
		if(bound == null) { true }
		else {
			val guess:GroundedRange = evaluateTemporal(ground,offset)
			(guess.begin >= bound.begin && guess.end <= bound.end)
		}
	}

	override def prob(ground:GroundedRange,rawOffset:Long):Double = {
		val offset = rawOffset //evaluate already incorporates new offset
		if(this.distrib == null)
			{ this.distrib = updater._2(this.toString,false) }
		val cand:Double = this.distrib( offset, diff(ground,offset,0) )
		assert(cand >= 0.0 && cand <= 1.0, "Invalid probability: " + cand)
		cand
	}


	
	def intersect(range:GroundedRange) = {
		if(range.begin == Time.END_OF || range.end == Time.DAWN_OF){
			//(case: intersecting with some invalid time)
			new NoTime
		} else if(this.bound == null){
			//(case: creating a new bound)
			new RepeatedRange(snapFn,base,interv,range,moveOffset).name(name)
		} else {
			//(case: refining an existing bound)
			val newBound = (range ^ bound).asInstanceOf[GroundedRange]
			newBound match {
				//((no such bound))
				case (nt:NoTime) => new NoTime
				//((new grounded range))
				case (gr:GroundedRange) =>
					new RepeatedRange(snapFn,base,interv,newBound,moveOffset).name(name)
				//((impossible)
				case _ => throw new IllegalStateException("bad intersect: " + newBound)
			}
		}
	}

	override def >>(diff:Duration):Range 
		= new RepeatedRange(snapFn, (base >> diff).asInstanceOf[UngroundedRange], 
				interv, bound, moveOffset).name(name)
	override def <<(diff:Duration):Range 
		= new RepeatedRange(snapFn, (base << diff).asInstanceOf[UngroundedRange], 
				interv, bound, moveOffset).name(name)
	override def <|(diff:Duration):Range 
		= new RepeatedRange(snapFn, (base <| diff).asInstanceOf[UngroundedRange], 
				interv, bound, moveOffset).name(name)
	override def |>(diff:Duration):Range 
		= new RepeatedRange(snapFn, (base |> diff).asInstanceOf[UngroundedRange], 
				interv, bound, moveOffset).name(name)
	override def |<(diff:Duration):Range 
		= new RepeatedRange(snapFn, (base |< diff).asInstanceOf[UngroundedRange], 
				interv, bound, moveOffset).name(name)
	override def >|(diff:Duration):Range 
		= new RepeatedRange(snapFn, (base >| diff).asInstanceOf[UngroundedRange], 
				interv, bound, moveOffset).name(name)
	override def !(dur:Duration):Range = this
	override def norm:GroundedDuration = interv.interval
	
	override def interval:GroundedDuration = interv.interval
	override def seconds:Long = interv.seconds

	override def units:Array[DurationUnit.Value] = base.norm.units

	override def +(diff:Duration):Duration 
		= new RepeatedRange(snapFn, base, interv + diff, bound, moveOffset)
			.name(name)
	override def -(diff:Duration):Duration 
		= new RepeatedRange(snapFn, base, interv - diff, bound, moveOffset)
			.name(name)
	override def *(n:Long):Duration 
		= new RepeatedRange(snapFn, base, interv * n, bound, moveOffset)
			.name(name)
	
	override def equals(o:Any):Boolean = { this eq o.asInstanceOf[AnyRef] }
	private var name:String = this.base.toString + " every " + interv
	def name(n:String):RepeatedRange = {this.name = n; this}
	override def toString:String = {
		if(bound == null){
			name+{
				if(moveOffset != 0L) {if(moveOffset > 0) "+" else ""}+moveOffset 
				else "" }
		} else {
			this.base.toString + " in " + bound
		}
	}
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}

// ----- OBJECT SEQUENCE -----
object Sequence {
	def apply(snapFn:Time=>Time,norm:Duration,interval:Duration)
		= new RepeatedRange(snapFn,Range(norm),interval,null,0L)
	
	type Distribution = (Long,Double)=>Double
	type Updater = ((Long,Double,Double)=>Unit,(String,Boolean)=>Distribution) 

	def mkGaussianUpdater:Updater = {
		var data:List[(Double,Double)] = List[(Double,Double)]()
		var dist:Distribution = null
		var seenE:Boolean = false
		//(updateE)
		val e = (o:Long,x:Double,logprob:Double) => { 
			assert(logprob <= 0.0, "Invalid log probability: " + logprob)
			data = (x,logprob) :: data 
			seenE = true
		}
		//(runM)
		val m = (tag:String,update:Boolean) => {
			if((update && seenE) || dist == null){
				//(parameters)
				assert(O.timeDistributionParams.length == 2, "Bad gaussian params")
				val muPrior:Double = O.timeDistributionParams(0)
				val sigmaPrior:Double = O.timeDistributionParams(1)
				//((mu))
				val (muNumer,muDenom) = data.foldLeft((muPrior,0.0)){ 
					case ((numer:Double,denom:Double),(x:Double,logprob:Double)) =>
						val prob = math.exp(logprob)
						val count = if(O.hardEM){ x } else { x*prob }
						(numer+count, denom+prob)
					}
				val mu:Double = if(muDenom == 0){ muPrior } else { muNumer / muDenom }
				//((sigma))
				val (sigmaNumer,sigmaDenom)=data.foldLeft((sigmaPrior*sigmaPrior,0.0)){ 
					case ((numer:Double,denom:Double),(x:Double,logprob:Double)) =>
						val prob = math.exp(logprob)
						val count = if(O.hardEM){ 1.0 } else { prob }
						(numer+count*(x-mu)*(x-mu), denom+count)
					}
				val sigmasq:Double
					= if(sigmaDenom == 0){ sigmaPrior*sigmaPrior } 
					  else { sigmaNumer / sigmaDenom }
				//(debug)
				assert(sigmasq > 0.0, "Sigma^2 is zero: " + sigmasq)
				assert(!sigmasq.isNaN, "Sigma^2 is NaN: " + sigmasq)
				assert(!mu.isNaN, "mu is NaN: " + mu)
				if(dist != null){
					val str = "Normalize ["+tag+"] ("+mu+","+math.sqrt(sigmasq)+")"
					if(O.printAllParses){ log(FORCE,str) } else { log(str) }
				}
				//(clear data)
				data = List[(Double,Double)]()
				//(distribution)
//				dist = (offset:Long,x:Double) => {
//					1.0 / math.sqrt(2.0*math.Pi*sigmasq) * 
//						math.exp( -1.0*(x-mu)*(x-mu) / (2.0*sigmasq) ) //<--value
//				}
				val distribImpl = new NormalDistributionImpl(mu,math.sqrt(sigmasq))
				dist = (offset:Long,x:Double) => {
					distribImpl.cumulativeProbability(x+1.0)-
					distribImpl.cumulativeProbability(x)               //<--CDF
				}
			}
			seenE = false
			dist
		}
		//(return)
		(e,m)
	}
	lazy val gaussianUpdater:Updater = mkGaussianUpdater

	def mkMultinomialUpdater:Updater = {
		var data = new ClassicCounter[Long]()
		var dist:Distribution = null
		var seenE:Boolean = false
		//(updateE)
		val e:(Long,Double,Double)=>Unit = (o:Long,x:Double,logprob:Double) => { 
			assert(logprob <= 0.0, "Invalid log probability: " + logprob)
			data.incrementCount(o,math.exp(logprob))
			seenE = true
		}
		//(runM)
		val m = (tag:String,update:Boolean) => {
			if((update && seenE) || dist == null){
				//(initialize)
				if(data.totalCount == 0.0){
					O.timeDistributionParams.zipWithIndex.foreach{ 
						case (c:java.lang.Double,i:Int) =>
							if(i == 0) { data.incrementCount(i,c) }
							else{ data.incrementCount(i,c); data.incrementCount(-i,c); }
					}
				}
				//(normalize)
				Counters.normalize(data)
				val counts:Counter[Long] = data
				data = new ClassicCounter[Long]()
				//(debug)
				if(dist != null){
					val str = "Normalize ["+tag+"] " + counts
					if(O.printAllParses){ log(FORCE,str) } else { log(str) }
				}
				//(distribution)
				dist = (offset:Long,x:Double) => counts.getCount(offset)
			}
			seenE = false
			dist
		}
		//(return)
		seenE = true
		(e,m)
	}
	lazy val multinomialUpdater:Updater = mkMultinomialUpdater

	lazy val pointUpdater:Updater = {
		//(updateE)
		val e:(Long,Double,Double)=>Unit = (o:Long,x:Double,logprob:Double) => { }
		//(runM)
		val m = (tag:String,update:Boolean) => 
			{(offset:Long,x:Double) => if(offset == 0){ 1.0 } else { 0.0 }}
		//(return)
		(e,m)
	}

	
}



//------------------------------------------------------------------------------
// TIME
//------------------------------------------------------------------------------
// ----- TIME -----
case class Time(base:DateTime) {
	def >(t:Time):Boolean = this.base.getMillis > t.base.getMillis
	def >=(t:Time):Boolean = !(this.base.getMillis < t.base.getMillis)
	def <(t:Time):Boolean = this.base.getMillis < t.base.getMillis
	def <=(t:Time):Boolean = !(this.base.getMillis > t.base.getMillis)

	def year:Int    = base.getYear
	def quarter:Int = base.getMonthOfYear/3 + 1
	def week:Int    = base.getWeekOfWeekyear
	def month:Int   = base.getMonthOfYear
	def day:Int     = base.getDayOfMonth
	def hour:Int    = base.getHourOfDay
	def minute:Int  = base.getMinuteOfHour
	def second:Int  = base.getSecondOfMinute

	def hasTime:Boolean = base.getMillisOfDay > 0


	def canonical(dur:Duration):Time = {
		def yr(t:DateTime):Time = {
			new Time(t.withMonthOfYear(1).withDayOfMonth(1).withMillisOfDay(0))
		}
		def hr(t:Time):Time = {
			new Time(t.base.withMinuteOfHour(0).withSecondOfMinute(0)
				.withMillisOfSecond(0))
		}
		def qr(t:Time):Time = {
			val month0 = t.base.getMonthOfYear-1
			new Time(t.base.withMonthOfYear( month0-(month0%3)+1 )
				.withDayOfMonth(1).withMillisOfDay(0))
		}
		dur.smallestUnit match {
			case DurationUnit.MILLENIUM => 
				yr(base.withYear(base.getYear-(base.getYear%1000)))
			case DurationUnit.CENTURY => 
				yr(base.withYear(base.getYear-(base.getYear%100)))
			case DurationUnit.DECADE => 
				yr(base.withYear(base.getYear-(base.getYear%10)))
			case DurationUnit.YEAR => yr(base)
			case DurationUnit.QUARTER => qr(this)
			case DurationUnit.MONTH => 
				new Time(base.withDayOfMonth(1).withMillisOfDay(0))
			case DurationUnit.WEEK => 
				new Time(base.withDayOfWeek(1).withMillisOfDay(0))
			case DurationUnit.DAY => new Time(base.withMillisOfDay(0))
			case DurationUnit.HOUR => hr(this)
			case DurationUnit.MINUTE => 
				new Time(base.withSecondOfMinute(0).withMillisOfSecond(0))
			case DurationUnit.SECOND => new Time(base.withMillisOfSecond(0))
			case DurationUnit.ZERO => this
		}
	}

	def +(diff:Duration):Time = {
		val diffMillis = diff.seconds*1000
		val baseMillis = base.getMillis
		if(diffMillis > 0 && baseMillis > Long.MaxValue-diffMillis){
			//((overflow))
			Time.END_OF
		} else if(diffMillis < 0 && baseMillis < Long.MinValue-diffMillis ){
			//((underflow))
			Time.DAWN_OF
		} else {
			//((normal))
			try{
				new Time(base.plus(diff.interval.base))
			} catch {
				case (e:ArithmeticException) =>
					new Time(base.plus(diffMillis))
				case (e:org.joda.time.IllegalFieldValueException) =>
					new Time(base.plus(diffMillis))
				case (e:ArrayIndexOutOfBoundsException) =>
					new Time(base.plus(diffMillis)) //TODO should not have to catch this
			}
		}
	}

	def -(diff:Duration):Time = {
		val diffMillis = diff.seconds*1000
		val baseMillis = base.getMillis
		if( diffMillis > 0 && baseMillis < Long.MinValue+diffMillis ) {
			//(underflow)
			new Time(new DateTime(Long.MinValue))
		} else if(diffMillis < 0 && baseMillis > Long.MaxValue+diffMillis) {
			//(overflow)
			new Time(new DateTime(Long.MaxValue))
		} else {
			//(normal)
			try{
				new Time(base.minus(diff.interval.base))
			} catch {
				case (e:ArithmeticException) =>
					new Time(base.plus(diffMillis))
				case (e:org.joda.time.IllegalFieldValueException) =>
					new Time(base.minus(diffMillis))
				case (e:ArrayIndexOutOfBoundsException) =>
					new Time(base.minus(diffMillis)) //TODO should not have to catch this
			}
		}
	}

	def -(other:Time):Duration = {
		assert(this.equals(Time.DAWN_OF) || !(this eq Time.DAWN_OF), "eq check")
		assert(this.equals(Time.END_OF) || !(this eq Time.END_OF), "eq check")
		val tM:Long = this.base.toInstant.getMillis
		val oM:Long = other.base.toInstant.getMillis
		if(this eq Time.DAWN_OF){
			//(case: subtracting from neg_infinity)
			if(other eq Time.DAWN_OF){ Duration.ZERO }
			else { Duration.NEG_INFINITE }
		} else if(this eq Time.END_OF){
			//(case: subtracting from pos_infinity)
			if(other eq Time.END_OF){ Duration.ZERO }
			else { Duration.INFINITE }
		} else if(oM < 0 && Long.MaxValue + oM < tM){
			//(case: overflowing a Long)
			Duration.INFINITE
		} else if(oM > 0 && Long.MinValue + oM > tM){
			//(case: underflowing a Long)
			Duration.NEG_INFINITE
		} else {
			//(case: normal subtraction)
			try {
				new GroundedDuration(new Period(tM-oM))
			} catch {
				//(case: overflowed precise fields)
				case (e:ArithmeticException) => 
					if(tM < 0 && oM > 0){ Duration.NEG_INFINITE }
					else if(tM > 0 && oM < 0){ Duration.INFINITE }
					else if(tM-oM > 0){ Duration.INFINITE }
					else if(tM-oM < 0){ Duration.NEG_INFINITE }
					else if(tM == oM){ Duration.ZERO }
					else{throw new TimeException("Unknown subtraction: "+this+" "+other)}
			}
		}
	}
	
	def guessRange:GroundedRange = {
		import Lex._
		if(second != 0){
			Range(this,this)
		} else if(minute != 0){
			Range(this,this+MIN)
		} else if(hour != 0){
			Range(this,this+HOUR)
		} else if(day != 1){
			Range(this,this+DAY)
		} else if((month-1) % 3 != 0){
			Range(this,this+MONTH)
		} else {
			Range(this,this+QUARTER)
		}
	}

	override def toString:String = this.base.toString
}

// ----- OBJECT TIME -----
object Time {
	val DAWN_OF:Time = new Time(new DateTime(Long.MinValue))
	val END_OF:Time  = new Time(new DateTime(Long.MaxValue))
	
	def apply(year:Int, month:Int, day:Int, hour:Int, min:Int, sec:Int):Time = {
		try{
			apply(new DateTime(year,month,day,hour,min,sec,0))
		} catch {
			case (e:org.joda.time.IllegalFieldValueException) =>
				if(year > 0){ Time.END_OF }else{ Time.DAWN_OF }
		}
	}
	def apply(year:Int, month:Int, day:Int, hour:Int, min:Int):Time = {
		apply(year, month, day, hour, min, 0)
	}
	def apply(year:Int, month:Int, day:Int, hour:Int):Time = {
		apply(year, month, day, hour, 0, 0)
	}
	def apply(year:Int, month:Int, day:Int):Time =apply(year, month, day, 0, 0, 0)
	def apply(year:Int, month:Int):Time = apply(year, month, 1, 0, 0, 0)
	def apply(year:Int):Time = apply(year, 1, 1, 0, 0,0)
	
	def main(args:Array[String]):Unit = {
		Temporal.interactive
	}
}


//------------------------------------------------------------------------------
// MISC
//------------------------------------------------------------------------------
// ----- TIME EXCEPTION -----
class TimeException(s:String,e:Throwable) extends RuntimeException(s,e) {
	def this() = this("",null)
	def this(s:String) = this(s,null)
	def this(e:Throwable) = this(null,e)
}

// ----- NO TIME -----
class NoTime extends GroundedRange(Time.DAWN_OF,Time.END_OF) with Sequence {
	override def evaluate[E <: Temporal](ground:GroundedRange,offset:Long):(TraverseFn,E)={
		this match {
			case (e:E) => ( (fn:TraverseTask) => fn(this,offset,0), e )
			case _ => throw new IllegalArgumentException("Runtime Type Error")
		}
	}
	override def prob(ground:GroundedRange,offset:Long):Double = 0.0
	override def exists(ground:GroundedRange,offset:Long):Boolean = false

	override def interval:GroundedDuration = new GroundedDuration(Seconds.ZERO)
	override def seconds:Long = 0L

	override def +(diff:Duration):Duration = this
	override def -(diff:Duration):Duration = this
	override def *(n:Long):Duration = this
	
	override def >>(diff:Duration):Range = this
	override def <<(diff:Duration):Range = this
	override def |>(diff:Duration):Range = this
	override def <|(diff:Duration):Range = this
	override def >|(diff:Duration):Range = this
	override def |<(diff:Duration):Range = this

	override def !(dur:Duration):Range = this
	override def units:Array[DurationUnit.Value] = Array[DurationUnit.Value]()

	override def move(offset:Long):Sequence = this

	override def toString = "NOTIME"
}

// ----- UNKNOWN TIME -----
class UnkTime extends NoTime {
	override def toString = "UNKTIME"
}

// ----- PARTIAL TIME -----
class PartialTime(fn:Range=>Range) extends Temporal {
	override def evaluate[E <: Temporal](ground:GroundedRange,offset:Long):(TraverseFn,E)={
		val resolved:Range = fn(Range(Time.DAWN_OF,Time.END_OF))
		val (tFn,grounded):(TraverseFn,GroundedRange) 
			= resolved.evaluate(ground,offset)
		grounded match {
			case (e:E) =>
				(Temporal.fnCat(tFn, (fn:TraverseTask) => fn(this,offset,0)),e)
			case _ => throw new IllegalArgumentException("Runtime Type Error")
		}
	}
	override def prob(ground:GroundedRange,offset:Long):Double
		= fn(Range(Time.DAWN_OF,Time.END_OF)).prob(ground,offset)
	override def exists(ground:GroundedRange,offset:Long):Boolean
		= fn(Range(Time.DAWN_OF,Time.END_OF)).exists(ground,offset)
}
object PartialTime {
	def apply(fn:Range=>Range) = new PartialTime(fn)
}

// ----- DURATION UNIT -----
object DurationUnit extends Enumeration {
	val MILLENIUM, CENTURY, DECADE, YEAR, QUARTER, MONTH, WEEK, DAY, 
		HOUR, MINUTE, SECOND, ZERO
			= Value
}

//------------------------------------------------------------------------------
// LEX
//------------------------------------------------------------------------------
object Lex {
	object LexUtil {
		def moh(iArg:Int):Time=>Time = (t:Time) => {
			val i:Int = if(iArg < 0) t.base.getHourOfDay else iArg
			Time(t.base.withMinuteOfHour(i).withSecondOfMinute(0))
		}
		def hod(iArg:Int):Time=>Time = (t:Time) => {
			val i:Int = if(iArg < 0) t.base.getHourOfDay else iArg
			Time(t.base.withHourOfDay( i ).
				withMinuteOfHour(0).withSecondOfMinute(0))
		}
		def dow(iArg:Int):Time=>Time = (t:Time) => {
			val i:Int = if(iArg < 0) t.base.getDayOfWeek else iArg
			Time(t.base.withDayOfWeek(i).withMillisOfDay(0))
		}
		def dom(iArg:Int):Time=>Time = (t:Time) => {
			val i:Int = if(iArg < 0) t.base.getDayOfWeek else iArg
			try {
				Time(t.base.withDayOfMonth(i).withMillisOfDay(0))
			} catch { case (e:org.joda.time.IllegalFieldValueException) =>
				Time(t.base.withDayOfMonth(1).withMillisOfDay(0))+MONTH-DAY
			}
		}
		def woy(iArg:Int):Time=>Time = (t:Time) => {
			val i:Int = if(iArg < 0) t.base.getDayOfWeek else iArg
			Time(t.base.withWeekOfWeekyear(i).withDayOfWeek(1).withMillisOfDay(0))
		}
		def moy(iArg:Int):Time=>Time = (t:Time) => {
			val i:Int = if(iArg < 0) t.base.getDayOfWeek else iArg
			Time(t.base.withMonthOfYear(i).withDayOfMonth(1).withMillisOfDay(0))
		}
		def qoy(iArg:Int):Time=>Time = (t:Time) => {
			val i:Int = if(iArg < 0) t.base.getDayOfWeek else iArg
			Time(t.base.withMonthOfYear(3*(i-1)+1)
				.withDayOfMonth(1).withMillisOfDay(0))
		}
		def yoc(iArg:Int):Time=>Time = (t:Time) => {
			val i:Int = if(iArg < 0) t.base.getDayOfWeek else iArg
			val newYear = t.base.getYear - (t.base.getYear%100) + i
			try{
				Time(t.base.withYear(newYear)
					.withMonthOfYear(1).withDayOfMonth(1).withMillisOfDay(0))
			} catch { case (e:IllegalFieldValueException) => 
				if(newYear < 0){
					Time.DAWN_OF
				}else{
					Time.END_OF
				}
			}
		}
		def yod(iArg:Int):Time=>Time = (t:Time) => {
			val i:Int = if(iArg < 0) t.base.getDayOfWeek else iArg
			val newYear = t.base.getYear - (t.base.getYear%10) + i
			try{
				Time(t.base.withYear(newYear)
					.withMonthOfYear(1).withDayOfMonth(1).withMillisOfDay(0))
			} catch { case (e:IllegalFieldValueException) => 
				if(newYear < 0){
					Time.DAWN_OF
				}else{
					Time.END_OF
				}
			}
		}
	}
	//--Durations
	val ASEC:Duration = new GroundedDuration(Seconds.ONE)
	val AMIN:Duration = new GroundedDuration(Minutes.ONE)
	val AHOUR:Duration = new GroundedDuration(Hours.ONE)
	val ADAY:Duration = new GroundedDuration(Days.ONE)
	val AWEEK:Duration = new GroundedDuration(Weeks.ONE)
	val AMONTH:Duration = new GroundedDuration(Months.ONE)
	val AQUARTER:Duration = new GroundedDuration(Months.THREE)
	val AYEAR:Duration = new GroundedDuration(Years.ONE)
	val ADECADE:Duration = new GroundedDuration(Years.ONE)*10
	val ACENTURY:Duration = new GroundedDuration(Years.ONE)*100
	//--Duration Sequences
	val SEC:Sequence 
		= new RepeatedRange((t:Time) => t.canonical(ASEC),Range(ASEC),ASEC)
			.dense.name("everySecond")
	val MIN:Sequence 
		= new RepeatedRange((t:Time) => t.canonical(AMIN),Range(AMIN),AMIN)
			.dense.name("everyMinute")
	val HOUR:Sequence 
		= new RepeatedRange((t:Time) => t.canonical(AHOUR),Range(AHOUR),AHOUR)
			.dense.name("everyHour")
	val DAY:Sequence 
		= new RepeatedRange((t:Time) => t.canonical(ADAY),Range(ADAY),ADAY)
			.dense.name("everyDay")
	val WEEK:Sequence 
		= new RepeatedRange((t:Time) => t.canonical(AWEEK),Range(AWEEK),AWEEK)
			.dense.name("everyWeek")
	val MONTH:Sequence 
		= new RepeatedRange((t:Time) => t.canonical(AMONTH),Range(AMONTH),AMONTH)
			.dense.name("everyMonth")
	val QUARTER:Sequence 
		= new RepeatedRange((t:Time) => 
			t.canonical(AQUARTER),Range(AQUARTER),AQUARTER)
			.dense.name("everyQuarter")
	val YEAR:Sequence 
		= new RepeatedRange((t:Time) => t.canonical(AYEAR),Range(AYEAR),AYEAR)
			.dense.name("everyYear")
	val DECADE:Sequence 
		= new RepeatedRange((t:Time) => t.canonical(ADECADE),Range(ADECADE),ADECADE)
			.dense.name("everyDecade")
	val CENTURY:Sequence 
		= new RepeatedRange((t:Time) => 
			t.canonical(ACENTURY),Range(ACENTURY),ACENTURY) 
			.dense.name("everyCentury")
	//--Misc
	val TODAY:Range = Range(DAY)
	val REF:Range = Range(Duration.ZERO)
	val ALL_TIME:Range = Range(Time.DAWN_OF,Time.END_OF)
	val PM:Range=>Range = _ >> HOUR*12
	//--Day of Week
	private def mkDOW(i:Int) = new RepeatedRange(
		LexUtil.dow(i), 
		Range(Duration(Days.ONE)), 
		Duration(Weeks.ONE)).dense
	val MON:Sequence = mkDOW(1)
	val TUE:Sequence = mkDOW(2)
	val WED:Sequence = mkDOW(3)
	val THU:Sequence = mkDOW(4)
	val FRI:Sequence = mkDOW(5)
	val SAT:Sequence = mkDOW(6)
	val SUN:Sequence = mkDOW(7)
	//--OTHER DurationS
	def HOD(i:Int) = new RepeatedRange(
		LexUtil.hod(i % 12), 
		Range(Duration(Hours.ONE)), 
		Duration(Days.ONE)).name("HOD("+(i%12)+")")
	def MOH(i:Int) = new RepeatedRange(
		LexUtil.moh(i), 
		Range(Duration(Minutes.ONE)), 
		Duration(Hours.ONE)).name("MOD("+i+")")
	def DOW(i:Int) = new RepeatedRange(
		LexUtil.dow(i), 
		Range(Duration(Days.ONE)), 
		Duration(Weeks.ONE)).name("DOW("+i+")")
	def DOM(i:Int) = new RepeatedRange(
		LexUtil.dom(i), 
		Range(Duration(Days.ONE)), 
		Duration(Months.ONE)).name("DOM("+i+")")
	def WOY(i:Int) = new RepeatedRange(
		LexUtil.woy(i), 
		Range(Duration(Weeks.ONE)), 
		Duration(Years.ONE)).name("WOY("+i+")")
	def MOY(i:Int) = new RepeatedRange(
		LexUtil.moy(i), 
		Range(Duration(Months.ONE)), 
		Duration(Years.ONE)).name("MOY("+i+")")
	def QOY(i:Int) = new RepeatedRange(
		LexUtil.qoy(i), 
		Range(Duration(Months.THREE)), 
		Duration(Years.ONE)).name("QOY("+i+")")
	def YOC(i:Int) = new RepeatedRange(
		LexUtil.yoc(i), 
		Range(Duration(Years.ONE)), 
		Duration(Years.years(100))).name("YOC("+i+")")
	def DOC(i:Int) = new RepeatedRange(
		LexUtil.yoc(i*10), 
		Range(Duration(Years.years(10))), 
		Duration(Years.years(100))).name("DOC("+i+")")
	def YOD(i:Int) = new RepeatedRange(
		LexUtil.yod(i), 
		Range(Duration(Years.ONE)), 
		Duration(Years.years(10))).name("YOD("+i+")")
	def THEYEAR(i:Int) = Range(Time(i),Time(i+1))
	def DECADE(i:Int) = Range(Time(i*10),Time((i+1)*10))
	def CENTURY(i:Int) = Range(Time(i*100),Time((i+1)*100))

	val YESTERDAY:Range = (REF <<! DAY)
	val TOMORROW:Range  = (REF >>! DAY)
	val FUTURE:Range    = Range(Duration.INFINITE)
	val PAST:Range      = Range(Duration.NEG_INFINITE)
	
	//--Functions
	//(move a range by a duration)
	val shiftLeft:(Range,Duration)=>Range = _ << _
	val shiftRight:(Range,Duration)=>Range = _ >> _
	//(cannonicalize a range)
	val canonicalize:(Range,Duration)=>Range = _ ! _
	//(move a range and canonicalize)
	val cannonicalLeft:(Range,Duration)=>Range = _ <<! _
	val cannonicalRight:(Range,Duration)=>Range = _ >>! _
	//(create a range on boundary -- outward)
	val catLeft:(Range,Duration)=>Range = _ <| _
	val catRight:(Range,Duration)=>Range = _ |> _
	//(create a range on boundary -- inward)
	val shrinkBegin:(Range,Duration)=>Range = _ |< _
	val shrinkEnd:(Range,Duration)=>Range = _ >| _
	//(intersect two ranges)
	val intersect:(Range,Range)=>Range = _ ^ _
	//(move a sequence one left or one right)
	val move:(Sequence,Long)=>Range = _ move _
	//(concatenate two ranges -- outer)
//	val cons:(Range,Range)=>Range = _.cons(_)
	//(concatenate two ranges -- inner)
	//TODO

	//(fuzzify a duration)
	val fuzzify:(Duration=>Duration) = ~_:Duration
	//(get norm of a range)
	val norm:(Range=>Duration) = _.norm

	def todaysDate:Time = Time((new DateTime).withMillisOfDay(0))
}



