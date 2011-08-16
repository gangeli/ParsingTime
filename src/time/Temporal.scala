package time

import org.joda.time._
import scala.collection.mutable.ArrayBuffer

//------------------------------------------------------------------------------
// TEMPORAL
//------------------------------------------------------------------------------
trait Temporal {
	import Temporal.isInt
	//(get values)
	def apply[E <: Temporal](offset:Int):Time=>E
	def prob(offset:Int):Time=>Double
	def exists(offset:Int):Time=>Boolean = (ground:Time) => (offset == 0)
	//(learn distribution)
	def updateE(logprob:Double):Unit = {}
	def runM:Unit = {}

	//(helper functions)
	final def forwardIterator[E <: Temporal](ground:Time):BufferedIterator[E] = {
		var rightPointer:Int = 0;
		new Iterator[E]{
			def hasNext:Boolean = Temporal.this.exists(rightPointer)(ground)
			def next:E = {
				val rtn = apply[E](rightPointer)(ground)
				assert(!rtn.isInstanceOf[NoTime], 
					"NoTime in iterator (hasNext: "+hasNext+")")
				rightPointer += 1
				rtn
			}
		}.buffered
	}
	final def backwardIterator[E <: Temporal](ground:Time):BufferedIterator[E] = {
		var leftPointer:Int = 0; //NOTE: will both hit zero
		new Iterator[E]{
			def hasNext:Boolean = Temporal.this.exists(leftPointer)(ground)
			def next:E = {
				val rtn = apply[E](leftPointer)(ground)
				assert(!rtn.isInstanceOf[NoTime], 
					"NoTime in iterator (hasNext: "+hasNext+")")
				leftPointer -= 1
				rtn
			}
		}.buffered
	}
	final def forwardIterable[E <: Temporal](ground:Time):Iterable[E] = {
		new Iterable[E]{ 
			def iterator:Iterator[E] = forwardIterator(ground)
		}
	}
	final def backwardIterable[E <: Temporal](ground:Time):Iterable[E] = {
		new Iterable[E]{ 
			def iterator:Iterator[E] = backwardIterator(ground)
		}
	}
	final def distribution(ground:Time):Iterable[(Temporal,Double,Int)] = {
		var leftPointer = -1;
		var rightPointer = 0;
		new Iterable[(Temporal,Double,Int)]{
			def iterator:Iterator[(Temporal,Double,Int)]
					= new Iterator[(Temporal,Double,Int)]{
				def hasNext:Boolean
					= Temporal.this.exists(leftPointer)(ground) || 
					  Temporal.this.exists(rightPointer)(ground)
				def next:(Temporal,Double,Int) = {
					assert(hasNext, "Calling next when there is no next")
					val pLeft = prob(leftPointer)(ground)
					val pRight = prob(rightPointer)(ground)
					if(Temporal.this.exists(leftPointer)(ground) && 
							(pLeft > pRight || !Temporal.this.exists(rightPointer)(ground))) {
						val rtn:Temporal = apply(leftPointer)(ground)
						assert(!rtn.isInstanceOf[NoTime], 
							"NoTime in distribution (hasNext: "+hasNext+")")
						leftPointer -= 1
						(rtn,pLeft,leftPointer+1)
					} else if(Temporal.this.exists(rightPointer)(ground)){
						val rtn:Temporal = apply(rightPointer)(ground)
						assert(!rtn.isInstanceOf[NoTime], 
							"NoTime in distribution (hasNext: "+hasNext+")")
						rightPointer += 1
						(rtn,pRight,rightPointer-1)
					} else {
						assert(!hasNext, "Inconsistent iterator")
						throw new NoSuchElementException()
					}
				}
			}
		}
	}
	final def apply(ground:Time):Temporal = {
		if(this.exists(0)(ground)) {
			apply(0)(ground)
		} else {
			new NoTime
		}
	}

	final def all(ground:Time):Array[Temporal] = {
		distribution(ground).map( _._1 ).toArray
	}

	final def timex3Type(ground:Time):String = this match {
//		case (s:Sequence) => "SET"
		case (r:Range) =>
			def getType(r:Range):String = {
				r match {
					case (gr:GroundedRange) => 
						if(gr.norm < Lex.DAY){ "TIME" } else { "DATE" }
					case _ => 
						r(ground) match {
							case (gr:GroundedRange) => getType(gr)
							case (nt:NoTime) => "UNK"
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

	final def timex3Value(ground:Time):String = {
		import Lex._
		import Temporal.{df2,df4}
		def getValue(r:Range):String = {
			r match {
				case (gr:GroundedRange) => 
					if(gr.begin == ground && gr.norm.seconds == 0){
						"PRESENT_REF"
					} else if(gr.begin == Time.DAWN_OF && gr.end != Time.END_OF){
						"FUTURE_REF"
					} else if(gr.end == Time.END_OF && gr.begin != Time.DAWN_OF){
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
		val d2 = d.toInt.toDouble
		d2 == d
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

	def <<!(dur:Duration):Range = (this << dur) ! dur
	def >>!(dur:Duration):Range = (this >> dur) ! dur

	private def composite[E <: Temporal](
			str:String,
			other:Range,
			fn:(Boolean,Iterable[E],Iterable[E])=>Iterable[(GroundedRange,Int,Int)]
				):CompositeRange = {
		var cacheBackward:(Int=>GroundedRange,Int=>(Int,Int),Int=>Boolean) = null
		var cacheForward:(Int=>GroundedRange,Int=>(Int,Int),Int=>Boolean) = null
		var cacheCond:Time = null
		def ensureCache(ground:Time) = {
			if(cacheForward == null  || ground != cacheCond){
				cacheCond = ground
				cacheForward = Range.iter2apply(fn(
						false, //forwards search
						this.forwardIterable(ground),
						other.forwardIterable(ground)
					).iterator)
				cacheBackward = 
					Range.iter2apply(fn(
						true, //backwards search
						this.backwardIterable(ground),
						other.backwardIterable(ground)
					).iterator)
			}
			val rtn = (cacheForward,cacheBackward)
			if(!O.cacheTemporalComputations){
				cacheForward = null
				cacheBackward = null
				cacheCond = null
			}
			rtn
		}
		new CompositeRange(
			//(new apply)
			(offset:Int) => { (ground:Time) => {
				val ((applyForward,indexForward,existsForward),
				     (applyBackward,indexBackward,existsBackward)) = ensureCache(ground)
				if(offset == 0) {
					if(existsForward(0)){
						applyForward(0)
					} else {
						applyBackward(0)
					}
				} else if(offset > 0){
					applyForward(offset)
				} else {
					applyBackward(-offset)
				}
			}},
			//(new prob)
			(offset:Int) => this.prob(offset),
			//(new exists)
			(offset:Int) => { (ground:Time) => {
				val ((applyForward,indexForward,existsForward),
				     (applyBackward,indexBackward,existsBackward)) = ensureCache(ground)
				if(offset == 0) {
					val forward = existsForward(0)
					if(forward){
						forward
					} else {
						existsBackward(0)
					}
				} else if(offset > 0){
					existsForward(offset)
				} else {
					existsBackward(-offset)
				}
			}},
			List[String]("("+this + ") "+str+" (" + other+")")
		)
	}

	def cons(other:Range):Range = {
		(this, other) match {
			case (a:Range,b:NoTime) => new NoTime
			case (a:GroundedRange,b:GroundedRange) => 
				if(a.begin < b.end){ new GroundedRange(a.begin,b.end) }
				else { new NoTime }
			case _ => composite("cons",other,
				Range.cons(_:Boolean,
					_:Iterable[GroundedRange],_:Iterable[GroundedRange]))
		}
	}

	def ^(other:Range):Range = {
		(this, other) match {
			case (a:Range,b:NoTime) => new NoTime
			case (a:GroundedRange,b:GroundedRange) => new GroundedRange(
					Range.mkBegin(a.begin,b.begin),
					Range.mkEnd(a.end,b.end)
				)
			case (a:GroundedRange,b:RepeatedRange) => b.intersect(a) //shortcut
			case (a:RepeatedRange,b:GroundedRange) => a.intersect(b) //.
			case _ => composite("^",other,
				Range.intersect(_:Boolean,
					_:Iterable[GroundedRange],_:Iterable[GroundedRange]))
		}
	}
}

// ----- COMPOSITE RANGE -----
class CompositeRange( 
			applyFn:Int=>(Time=>Range),
			probFn:Int=>(Time=>Double),
			existsFn:Int=>(Time=>Boolean),
			ops:List[String]
		) extends Range {
	
	override def apply[E <: Temporal](offset:Int):Time=>E = {
		applyFn(offset) match {
			case (fn:(Time=>E)) => (ground:Time) => {
				assert(fn(ground).isInstanceOf[GroundedRange], "Composite ungrounded")
				fn(ground)
			}
			case _ => throw new IllegalArgumentException("Runtime Type Error")
		}
	}
	override def prob(offset:Int):Time=>Double = probFn(offset)
	override def exists(offset:Int):Time=>Boolean = existsFn(offset)

	override def >>(diff:Duration):Range = extend( _ >> diff, ">>" )
	override def <<(diff:Duration):Range = extend( _ << diff, "<<" )
	override def |>(diff:Duration):Range = extend( _ |> diff, "|>" )
	override def <|(diff:Duration):Range = extend( _ <| diff, "<|" )
	override def >|(diff:Duration):Range = extend( _ >| diff, ">|" )
	override def |<(diff:Duration):Range = extend( _ |< diff, "|<" )
	override def !(diff:Duration):Range = extend( _ ! diff, "!" )

	private def extend(fn:Range=>Range, op:String) = {
		new CompositeRange( 
			(offset:Int) => { (ground:Time) => fn( this.applyFn(offset)(ground) ) },
			(offset:Int) => this.probFn(offset),
			(offset:Int) => this.existsFn(offset),
			op :: this.ops
		)
	}
	
	override def equals(o:Any):Boolean = this == o
	override def toString:String = Temporal.join(ops.toArray, " <- ")
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}

// ----- GROUNDED RANGE -----
class GroundedRange(val begin:Time,val end:Time) extends Range {
	override def apply[E <: Temporal](offset:Int):Time=>E = {
		if(offset == 0){ (t:Time) => 
			this match{ 
				case (e:E) => e
				case _ => throw new IllegalArgumentException("Runtime Type Error")
			}
		}
		else{throw new TimeException("GroundedRange given nonzero offset: "+offset)}
	}
	override def prob(offset:Int):Time=>Double = (t:Time) =>
		if(offset == 0){ 1.0 } else{ 0.0 }

	
	override def >>(diff:Duration):Range = new GroundedRange(begin+diff,end+diff)
	override def <<(diff:Duration):Range = new GroundedRange(begin-diff,end-diff)
	override def <|(diff:Duration):Range = new GroundedRange(begin-diff,begin)
	override def |>(diff:Duration):Range = new GroundedRange(end,end+diff)
	override def |<(diff:Duration):Range = new GroundedRange(begin,begin+diff)
	override def >|(diff:Duration):Range = new GroundedRange(end-diff,end)

	override def !(dur:Duration):Range = {
		import Lex.{AYEAR,QUARTER,MONTH,WEEK,DAY,HOUR,MIN,SEC}
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
		val (base,diff) = dur.smallestUnit match {
			case DurationUnit.MILLENIUM => 
				val b = begin.base
				( yr(b.withYear(b.getYear-(b.getYear%1000))), AYEAR*1000 )
			case DurationUnit.CENTURY => 
				val b = begin.base
				( yr(b.withYear(b.getYear-(b.getYear%100))),AYEAR*100)
			case DurationUnit.DECADE => 
				val b = begin.base
				( yr(b.withYear(b.getYear-(b.getYear%10))), AYEAR*10)
			case DurationUnit.YEAR => 
				( yr(begin.base), AYEAR)
			case DurationUnit.QUARTER => 
				( qr(begin), QUARTER )
			case DurationUnit.MONTH => 
				( new Time(begin.base.withDayOfMonth(1).withMillisOfDay(0)), MONTH)
			case DurationUnit.WEEK => 
				( new Time(begin.base.withDayOfWeek(1).withMillisOfDay(0)), WEEK)
			case DurationUnit.DAY => 
				( new Time(begin.base.withMillisOfDay(0)), DAY)
			case DurationUnit.HOUR => 
				( hr(begin), HOUR)
			case DurationUnit.MINUTE => 
				( new Time(begin.base.withSecondOfMinute(0).withMillisOfSecond(0)), MIN)
			case DurationUnit.SECOND => 
				( new Time(begin.base.withMillisOfSecond(0)), SEC)
			case DurationUnit.ZERO =>
				( begin, Duration.ZERO )
		}
		new GroundedRange(base,base+diff)
	}

	
	def norm:Duration = (end - begin)

	override def equals(o:Any):Boolean = o match {
		case (gr:GroundedRange) => 
			gr.begin.equals(this.begin) && gr.end.equals(this.end)
		case _ => false
	}
	override def toString:String = "["+begin+", "+end+")"
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}


// ----- UNGROUNDED RANGE -----
class UngroundedRange(val normVal:Duration,val beginOffset:Duration
		) extends Range{
	override def apply[E <: Temporal](offset:Int) = {
		if(offset == 0){ 
			(ground:Time) =>
				new GroundedRange(ground+beginOffset,ground+beginOffset+normVal) match {
					case (e:E) => e
					case _ => throw new IllegalArgumentException("Runtime Type Error")
				}
		} else{
			throw new TimeException("UngroundedRange given nonzero offset: "+offset)
		}
	}
	override def prob(offset:Int):Time=>Double = (t:Time) =>
		if(offset == 0){ 1.0 } else{ 0.0 }

	override def >>(diff:Duration):Range 
		= new UngroundedRange(normVal,beginOffset+diff)

	override def <<(diff:Duration):Range 
		= new UngroundedRange(normVal,beginOffset-diff)

	override def <|(diff:Duration):Range 
		= new UngroundedRange(diff,beginOffset-diff)

	override def |>(diff:Duration):Range 
		= new UngroundedRange(diff,beginOffset+normVal)

	override def |<(diff:Duration):Range 
		= new UngroundedRange(diff,beginOffset)

	override def >|(diff:Duration):Range 
		= new UngroundedRange(diff,beginOffset+normVal-diff)
	
	def norm:Duration = normVal
	
	override def !(dur:Duration):Range = {
		new CompositeRange(
				(offset:Int) => (t:Time) => {
					val gr:GroundedRange = this.apply(offset)(t)
					gr ! dur
				},
				this.prob(_),
				this.exists(_),
				List[String](this + " ! " + dur)
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
	def apply(begin:Range,end:Range) = begin cons end
	def apply(norm:Duration) = new UngroundedRange(norm,Duration.ZERO)
	def apply(norm:Period) = new UngroundedRange(Duration(norm),Duration.ZERO)

	def iter2apply(iter:Iterator[(GroundedRange,Int,Int)]
			):(Int=>GroundedRange,Int=>(Int,Int),Int=>Boolean) = {
		val buffer = new ArrayBuffer[(GroundedRange,Int,Int)]()
		(
			(in:Int) => {
				while(in >= buffer.length && iter.hasNext){ buffer.append(iter.next) }
				if(in < buffer.length){ buffer(in)._1 } 
				else{ throw new IllegalArgumentException("Out of bounds index: " + in +
					" (has next? " + iter.hasNext + ")") }
			},
			(in:Int) => {
				while(in >= buffer.length && iter.hasNext){ buffer.append(iter.next) }
				if(in < buffer.length){ (buffer(in)._2,buffer(in)._3) } 
				else{ throw new IllegalArgumentException("Out of bounds index: " + in +
					" (has next? " + iter.hasNext + ")") }
			},
			(in:Int) => {
				while(in >= buffer.length && iter.hasNext){ buffer.append(iter.next) }
				in < buffer.length
			}
		)
	}

	def mkBegin(a:Time,b:Time) = if(a < b) b else a
	def mkEnd(a:Time,b:Time) = if(a < b) a else b

	def mknext2iterable( 
			a:Iterable[GroundedRange], b:Iterable[GroundedRange],
			mkNext:(Int,GroundedRange,BufferedIterator[GroundedRange],
			        Int,GroundedRange,BufferedIterator[GroundedRange])
							=>
							(GroundedRange,
							((GroundedRange,Int),(GroundedRange,Int)),
							(Int,Int)) ):Iterable[(GroundedRange,Int,Int)] = {
		new Iterable[(GroundedRange,Int,Int)]{
			def iterator:Iterator[(GroundedRange,Int,Int)] = {
				new Iterator[(GroundedRange,Int,Int)]{
					private val iterA = a.iterator.buffered
					private val iterB = b.iterator.buffered
					private var (theNext,((headA,iA),(headB,iB)),(indexA,indexB))
						:(GroundedRange,((GroundedRange,Int),(GroundedRange,Int)),(Int,Int))
						= if(iterA.hasNext && iterB.hasNext){
								mkNext(0,iterA.next,iterA,0,iterB.next,iterB)
							} else {
								(null,((null,0),(null,0)),(0,0))
							}
					override def hasNext:Boolean 
						= theNext != null
					override def next:(GroundedRange,Int,Int) = {
						if(theNext == null){ throw new NoSuchElementException }
						val rtn=(theNext,indexA,indexB)
						val (n,((vA,inA),(vB,inB)),(indA,indB))
							= mkNext(iA,headA,iterA,iB,headB,iterB)
						theNext=n;headA=vA;headB=vB;iA=inA;iB=inB;indexA=indA;indexB=indB
						rtn
					}
				}
			}
		}
		
	}

	def cons(back:Boolean,a:Iterable[GroundedRange],b:Iterable[GroundedRange]
			):Iterable[(GroundedRange,Int,Int)] = {
		var diff:Duration = Duration.INFINITE
		def mkNext(
				iA:Int, vA:GroundedRange,a:BufferedIterator[GroundedRange],
				iB:Int, vB:GroundedRange,b:BufferedIterator[GroundedRange]
				):(GroundedRange,((GroundedRange,Int),(GroundedRange,Int)),(Int,Int))={
			val nullVal = (null,((null,iA),(null,iB)),(iA,iB))
			if(vA==null || vB==null){
				//(case: an iterator is empty)
				nullVal
			} else if(vB.end < vA.begin){
				//(case: B is behind)
				//((convergence check))
				val lastDiff = diff
				diff = (vA.begin-vB.end)
				if(!(diff < lastDiff)){ nullVal } //case: not converging
				//((movement))
				else if(!back && b.hasNext){ mkNext(iA,vA,a,iB+1,b.next,b) } 
				else if(back && a.hasNext){ mkNext(iA+1,a.next,a,iB,vB,b) } 
				else { nullVal }
			} else {
				//(case: overlap)
				val rtn = new GroundedRange(vA.begin,vB.end)
				//(update iterator)
				if(a.hasNext){
					//(case: A can jump again)
					(rtn,((a.next,iA+1),(vB,iB)),(iA,iB))
				} else {
					(rtn,((null,iA),(vB,iB)),(iA,iB))
				}
			}
		}
		mknext2iterable(a,b,mkNext(_,_,_,_,_,_))
	}

	def intersect(back:Boolean,a:Iterable[GroundedRange],b:Iterable[GroundedRange]
			):Iterable[(GroundedRange,Int,Int)] = {
		var diff:Duration = Duration.INFINITE
		def mkNext(
				iA:Int, vA:GroundedRange,a:BufferedIterator[GroundedRange],
				iB:Int, vB:GroundedRange,b:BufferedIterator[GroundedRange]
				):(GroundedRange,((GroundedRange,Int),(GroundedRange,Int)),(Int,Int))={
			val nullVal = (null,((null,iA),(null,iB)),(iA,iB))
			if(vA==null || vB==null){
				//(case: an iterator is empty)
				nullVal
			} else if(vA.end <= vB.begin) {
				//(case: A is before)
				//((overhead for divergence))
				val lastDiff = diff
				diff = (vB.begin-vA.end)
				if(!(diff < lastDiff)){ nullVal } //case: not converging
				//((movement))
				else if(!back && a.hasNext){ mkNext(iA+1,a.next,a,iB,vB,b) } 
				else if(back && b.hasNext){ mkNext(iA,vA,a,iB+1,b.next,b) } 
				else { nullVal } //case: relevant iterator is empty
			} else if(vB.end <= vA.begin){
				//(case: B is before)
				//((overhead for divergence))
				val lastDiff = diff
				diff = vA.begin-vB.end
				if(!(diff < lastDiff)){ nullVal } //case: not converging
				//((movement))
				else if(!back && b.hasNext){ mkNext(iA,vA,a,iB+1,b.next,b) } 
				else if(back && a.hasNext){ mkNext(iA+1,a.next,a,iB,vB,b) } 
				else { nullVal } //case: relevant iterator is empty
			} else {
				//(case: overlap)
				diff = Duration.INFINITE //reset convergence criteria
				val rtn = new GroundedRange(
						mkBegin(vA.begin,vB.begin),
						mkEnd(vA.end,vB.end)
					)
				//(update iterator)
				if(a.hasNext && !(a.head.begin > vB.end)){
					//(case: A can jump again)
					(rtn,((a.next,iA+1),(vB,iB)),(iA,iB))
				} else if(b.hasNext) {
					//(case: B can either jump, or we jump it anyways)
					(rtn,((vA,iA),(b.next,iB+1)),(iA,iB))
				} else {
					(rtn,((vA,iA),(null,iB)),(iA,iB))
				}
			}
		}
		//(call the bloody function)
		mknext2iterable(a,b,mkNext(_,_,_,_,_,_))
	}
}

//------------------------------------------------------------------------------
// DURATION
//------------------------------------------------------------------------------
// ----- DURATION -----
trait Duration extends Temporal {
	override def apply[E <: Temporal](offset:Int):Time=>E = {
		if(offset == 0){ (t:Time) => this match {
				case (e:E) => 
					assert(e.isInstanceOf[GroundedDuration] || 
						e.isInstanceOf[FuzzyDuration], 
						"Duration not grounded")
					e
				case _ => throw new IllegalArgumentException("Runtime Type Error")
			}
		}
		else{ throw new TimeException("Duration given nonzero offset: "+offset) }
	}
	override def prob(offset:Int):Time=>Double = (t:Time) =>
		if(offset == 0){ 1.0 } else{ 0.0 }

	def interval:GroundedDuration
	def seconds:Long

	def +(diff:Duration):Duration 
	def -(diff:Duration):Duration
	def *(n:Int):Duration
	
	def /(other:Duration):Double 
		= this.seconds.asInstanceOf[Double] / other.seconds.asInstanceOf[Double]
	def <(other:Duration) = this.seconds < other.seconds
	def <=(other:Duration) = this.seconds <= other.seconds
	def >=(other:Duration) = this.seconds >= other.seconds

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
		= new GroundedDuration(this.base.toPeriod.plus(diff.interval.base))
	override def -(diff:Duration):Duration 
		= new GroundedDuration(base.toPeriod.minus(diff.interval.base))
	override def *(n:Int):Duration = {
		import DurationUnit._
		var p = base.toPeriod
		new GroundedDuration( units.foldLeft(Period.ZERO){ 
			case (soFar:Period,term:DurationUnit.Value) => term match {
				case MILLENIUM => soFar.plusYears((p.getYears/1000)*n)
				case CENTURY => soFar.plusYears((p.getYears%1000)*n)
				case DECADE => soFar.plusYears((p.getYears%100)*n)
				case YEAR => soFar.plusYears((p.getYears%10)*n)
				case QUARTER => soFar.plusMonths(p.getMonths*n)
				case MONTH => soFar.plusMonths(p.getMonths*n)
				case WEEK => soFar.plusWeeks(p.getWeeks*n)
				case DAY => soFar.plusDays(p.getDays*n)
				case HOUR => soFar.plusHours(p.getHours*n)
				case MINUTE => soFar.plusMinutes(p.getMinutes*n)
				case SECOND => soFar.plusSeconds(p.getSeconds*n)
			}
		} )
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
	override def *(n:Int):Duration         = this.base * n

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
	override def exists(offset:Int) = (ground:Time) => true
	override def equals(o:Any):Boolean = this == o
}

// ----- REPEATED RANGE -----
class RepeatedRange(snapFn:Time=>Time,base:UngroundedRange,interv:Duration,
		bound:GroundedRange) extends Sequence {

	def this(snapFn:Time=>Time,base:UngroundedRange,interv:Duration) 
		= this(snapFn,base,interv,null)
	def this(snapFn:Time=>Time,base:Range,interv:Duration)
		= this(
			snapFn,
			base match {
				case (b:UngroundedRange) => b
				case _ => throw new IllegalArgumentException("Runtime Type Error")
			},
			interv)
	
	override def exists(offset:Int) = (ground:Time) => {
		if(bound == null){ true }
		else{
			val guess:GroundedRange = apply(offset)(ground)
			(guess.begin >= bound.begin && guess.end <= bound.end)
		}
	}
	
	override def apply[E <: Temporal](offset:Int):Time=>E = {
		var cache:Temporal = null; var cacheCond:Time = null
		(ground:Time) => {
			val term = 
				if( cache == null || ground != cacheCond) {
					//(update cache condition)
					cacheCond = ground
					//(get start)
					val beginT:Time = 
						if(bound == null) {
							ground
						} else {
							if(ground > bound.begin && ground < bound.end){ ground }
							else { bound.begin }
						}
					//(snap beginning)
					val begin:Time = 
						if(bound == null){ snapFn(beginT+interv*offset) } //interv first
						else{ snapFn(beginT+interv*offset) } //^ note above
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
			term match {
				case (e:E) => 
					assert(e.isInstanceOf[GroundedRange], "Range not grounded")
					e
				case _ => throw new IllegalArgumentException("Runtime Type Error")
			}
		}
	}

	override def prob(offset:Int):Time=>Double = {
		import Sequence._
		val groundFn:Time=>GroundedRange = this.apply(offset)
		def diff(offset:Int,ground:Time):Double = {
			val grounded:GroundedRange = this.apply(offset)(ground)
			math.abs((ground - grounded.begin)/interv)
		}
		(ground:Time) =>
			//(distribution)
			if(!this.exists(offset)(ground)){
				0.0
			} else if(bound == null){
				//(calculate norm)
				val expNorm2Dir:Double = (-4 until 5).foldLeft(0.0){ 
					case (soFar:Double,o:Int) =>
					if(exists(o)(ground)) { 
						soFar + expLambda*math.exp(-expLambda*diff(o,ground)) 
					} else { 
						soFar 
					} }
				//(take probability)
				if(math.abs(offset) < 5){
					expLambda*math.exp(-expLambda*diff(offset,ground)) / expNorm2Dir
				} else {
					0.0
				}
			} else {
				//(calculate norm)
				val expNorm1Dir:Double = (0 until 5).foldLeft(0.0){ 
					case (soFar:Double,o:Int) =>
					if(exists(o)(ground)) { 
						soFar + expLambda*math.exp(-expLambda*diff(o,ground)) 
					} else { 
						soFar 
					} }
				//(take probability)
				if(offset >= 0 && offset < 5){
					expLambda*math.exp(expLambda*diff(offset,ground)) / expNorm1Dir
				} else {
					0.0
				}
				math.pow((1.0-geomP),math.abs(offset)) * geomP
			}
	}


	
	def intersect(range:GroundedRange) = {
		if(this.bound == null){
			new RepeatedRange(snapFn,base,interv,range)
		} else {
			val newBound:GroundedRange = (range ^ bound).asInstanceOf[GroundedRange]
			new RepeatedRange(snapFn,base,interv,newBound)
		}
	}

	override def >>(diff:Duration):Range 
		= new RepeatedRange(snapFn, base >> diff,interv)
	override def <<(diff:Duration):Range 
		= new RepeatedRange(snapFn, base << diff,interv)
	override def <|(diff:Duration):Range 
		= new RepeatedRange(snapFn, base <| diff, interv)
	override def |>(diff:Duration):Range 
		= new RepeatedRange(snapFn, base |> diff, interv)
	override def |<(diff:Duration):Range 
		= new RepeatedRange(snapFn, base |< diff, interv)
	override def >|(diff:Duration):Range 
		= new RepeatedRange(snapFn, base >| diff, interv)

	override def !(dur:Duration):Range = this
	
	override def interval:GroundedDuration = interv.interval
	override def seconds:Long = interv.seconds

	override def units:Array[DurationUnit.Value] = base.norm.units

	override def +(diff:Duration):Duration 
		= new RepeatedRange(snapFn, base, interv + diff)
	override def -(diff:Duration):Duration 
		= new RepeatedRange(snapFn, base, interv - diff)
	override def *(n:Int):Duration 
		= new RepeatedRange(snapFn, base, interv * n)
	
	override def equals(o:Any):Boolean = this == o
	override def toString:String 
		= this.base.toString + " every " + interv
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}

// ----- OBJECT SEQUENCE -----
object Sequence {
	def apply(snapFn:Time=>Time,norm:Duration,interval:Duration)
		= new RepeatedRange(snapFn,Range(norm),interval)
	val geomP:Double = 0.5
	val expLambda:Double = 1.0

	
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
	def quarter:Int = base.getMonthOfYear.toInt/3 + 1
	def week:Int    = base.getWeekOfWeekyear
	def month:Int   = base.getMonthOfYear
	def day:Int     = base.getDayOfMonth
	def hour:Int    = base.getHourOfDay
	def minute:Int  = base.getMinuteOfHour
	def second:Int  = base.getSecondOfMinute

	def hasTime:Boolean = base.getMillisOfDay > 0

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
					new Time(base.plus(diffMillis)) //catch-all
				case (e:org.joda.time.IllegalFieldValueException) =>
					new Time(base.plus(diffMillis)) //catch-all
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
					new Time(base.plus(diffMillis)) //catch-all
				case (e:org.joda.time.IllegalFieldValueException) =>
					new Time(base.minus(diffMillis)) //catch-all
			}
		}
	}

	def -(other:Time):Duration = {
		assert(this.equals(Time.DAWN_OF) || this != Time.DAWN_OF, "eq check")
		assert(this.equals(Time.END_OF) || this != Time.END_OF, "eq check")
		val tM:Long = this.base.toInstant.getMillis
		val oM:Long = other.base.toInstant.getMillis
		if(this == Time.DAWN_OF){
			//(case: subtracting from neg_infinity)
			if(other == Time.DAWN_OF){ Duration.ZERO }
			else { Duration.NEG_INFINITE }
		} else if(this == Time.END_OF){
			//(case: subtracting from pos_infinity)
			if(other == Time.END_OF){ Duration.ZERO }
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
class NoTime extends Sequence {
	override def apply[E <: Temporal](offset:Int) = (ground:Time) =>
		this match {
			case (e:E) => e
			case _ => throw new IllegalArgumentException("Runtime Type Error")
		}
	override def prob(offset:Int):Time=>Double = (t:Time) => 0.0
	override def exists(offset:Int):Time=>Boolean = (t:Time) => false

	def interval:GroundedDuration = new GroundedDuration(Seconds.ZERO)
	def seconds:Long = 0L

	def +(diff:Duration):Duration = this
	def -(diff:Duration):Duration = this
	def *(n:Int):Duration = this
	
	def >>(diff:Duration):Range = this
	def <<(diff:Duration):Range = this
	def |>(diff:Duration):Range = this
	def <|(diff:Duration):Range = this
	def >|(diff:Duration):Range = this
	def |<(diff:Duration):Range = this

	def !(dur:Duration):Range = this
	def units:Array[DurationUnit.Value] = Array[DurationUnit.Value]()

	override def toString = "NOTIME"
}

// ----- UNKNOWN TIME -----
class UnkTime extends NoTime {
	override def toString = "UNKTIME"
}

// ----- PARTIAL TIME -----
class PartialTime(fn:Range=>Range) extends Temporal {
	override def apply[E <: Temporal](offset:Int):Time=>E = {
		(ground:Time) =>  {
			val resolved:Range = fn(Range(Time.DAWN_OF,Time.END_OF))
			val grounded:E = resolved(offset)(ground)
			assert(grounded.isInstanceOf[GroundedRange], "Ungrounded PartialTime")
			grounded
		}
	}
	override def prob(offset:Int):Time=>Double
		= fn(Range(Time.DAWN_OF,Time.END_OF)).prob(offset)
	override def exists(offset:Int):Time=>Boolean
		= fn(Range(Time.DAWN_OF,Time.END_OF)).exists(offset)
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
		def hod(iArg:Int):Time=>Time = (t:Time) => {
			val i:Int = if(iArg < 0) t.base.getHourOfDay else iArg
			Time(t.base.withHourOfDay(i).withMinuteOfHour(0).withSecondOfMinute(0))
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
	val SEC:Duration = new GroundedDuration(Seconds.ONE)
	val MIN:Duration = new GroundedDuration(Minutes.ONE)
	val HOUR:Duration = new GroundedDuration(Hours.ONE)
	val DAY:Duration = new GroundedDuration(Days.ONE)
	val WEEK:Duration = new GroundedDuration(Weeks.ONE)
	val MONTH:Duration = new GroundedDuration(Months.ONE)
	val QUARTER:Duration = new GroundedDuration(Months.THREE)
	val AYEAR:Duration = new GroundedDuration(Years.ONE)
	//--Misc
	val TODAY:Range = Range(DAY)
	val REF:Range = Range(Duration.ZERO)
	val ALL_TIME:Range = Range(Time.DAWN_OF,Time.END_OF)
	//--Day of Week
	private def mkDOW(i:Int) = new RepeatedRange(
		LexUtil.dow(i), 
		Range(Duration(Days.ONE)), 
		Duration(Weeks.ONE))
	val MON:Sequence = mkDOW(1)
	val TUE:Sequence = mkDOW(2)
	val WED:Sequence = mkDOW(3)
	val THU:Sequence = mkDOW(4)
	val FRI:Sequence = mkDOW(5)
	val SAT:Sequence = mkDOW(6)
	val SUN:Sequence = mkDOW(7)
	//--OTHER DurationS
	def HOD(i:Int) = new RepeatedRange(
		LexUtil.hod(i-1), 
		Range(Duration(Hours.ONE)), 
		Duration(Days.ONE))
	def DOW(i:Int) = new RepeatedRange(
		LexUtil.dow(i), 
		Range(Duration(Days.ONE)), 
		Duration(Weeks.ONE))
	def DOM(i:Int) = new RepeatedRange(
		LexUtil.dom(i), 
		Range(Duration(Days.ONE)), 
		Duration(Months.ONE))
	def WOY(i:Int) = new RepeatedRange(
		LexUtil.woy(i), 
		Range(Duration(Weeks.ONE)), 
		Duration(Years.ONE))
	def MOY(i:Int) = new RepeatedRange(
		LexUtil.moy(i), 
		Range(Duration(Months.ONE)), 
		Duration(Years.ONE))
	def QOY(i:Int) = new RepeatedRange(
		LexUtil.qoy(i), 
		Range(Duration(Months.THREE)), 
		Duration(Years.ONE))
	def YOC(i:Int) = new RepeatedRange(
		LexUtil.yoc(i), 
		Range(Duration(Years.ONE)), 
		Duration(Years.years(100)))
	def DOC(i:Int) = new RepeatedRange(
		LexUtil.yoc(i*10), 
		Range(Duration(Years.years(10))), 
		Duration(Years.years(100)))
	def YOD(i:Int) = new RepeatedRange(
		LexUtil.yod(i), 
		Range(Duration(Years.ONE)), 
		Duration(Years.years(10)))
	def THEYEAR(i:Int) = Range(Time(i),Time(i+1))
	def DECADE(i:Int) = Range(Time(i*10),Time((i+1)*10))
	def CENTURY(i:Int) = Range(Time(i*100),Time((i+1)*100))

	val YESTERDAY:Range = (REF <<! DAY)
	val TOMORROW:Range = (REF >>! DAY)
	
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
	//(concatenate two ranges -- outer)
	val cons:(Range,Range)=>Range = _.cons(_)
	//(concatenate two ranges -- inner)
	//TODO

	//(fuzzify a duration)
	val fuzzify:(Duration=>Duration) = ~_:Duration

	def todaysDate:Time = Time((new DateTime).withMillisOfDay(0))
}



