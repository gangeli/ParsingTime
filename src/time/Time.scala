package time

import Conversions._
import Lex._
import edu.stanford.nlp.ie.temporal.timebank.Timex
import org.joda.time._

//TODO hash codes

//------------------------------------------------------------------------------
// UTILS
//------------------------------------------------------------------------------
object Conversions {
	implicit def period2offset(p:ReadablePeriod):Offset = new Offset(p)
	implicit def offset2period(o:Offset):ReadablePeriod = o.p
	implicit def instant2long(i:Instant):Long = i.getMillis
	implicit def long2instant(l:Long):Instant = new Instant(l)
}

//------------------------------------------------------------------------------
// TIME CLASSES
//------------------------------------------------------------------------------

// --- EXPRESSION ---


// --- SEQUENCE ---
trait Sequence {
	def base:Range
	def offset(n:Int):Range
	def isSingleton:Boolean
}
case class ConstantOffsetSequence(baseRange:Range, incr:Offset) 
		extends Sequence {
	override def base:Range = baseRange
	override def offset(n:Int):Range = baseRange >> incr*n
	override def isSingleton:Boolean = false
}
case class FixedSequence(ranges:Array[Range], b:Int) extends Sequence {
	override def base:Range = ranges(b)
	override def offset(n:Int):Range = ranges(b+n)
	override def isSingleton:Boolean = ranges.length == 1
}
case class SingletonSequence(range:Range) extends Sequence {
	override def base:Range = range
	override def offset(n:Int):Range = 
		throw new IllegalArgumentException("Cannot offset singleton sequence")
	override def isSingleton:Boolean = true
}

// --- RANGE ---
case class Range(begin:Time, end:Time){
	def >>(diff:Offset) = new Range(begin+diff, end+diff)
	def <<(diff:Offset) = new Range(begin-diff, end-diff)
	def <|(diff:Offset) = new Range(begin-diff, end)
	def |>(diff:Offset) = new Range(begin, end+diff)
	def |<(diff:Offset) = new Range(begin, end-diff)
	def >|(diff:Offset) = new Range(begin+diff, end)
	def apply(ground:Time) = {
		new Range(
				{if(!begin.isGrounded) begin(ground) else begin},
				{if(!end.isGrounded) end(ground) else end}
			)
	}
	def ~(o:Any):Boolean = {
		if(o.isInstanceOf[Range]){
			val other:Range = o.asInstanceOf[Range]
			return (this.begin ~ other.begin) && (this.end ~ other.end)
		}
		return false
	}
	override def toString:String = "(" + begin + ", " + end + ")"
	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[Range]){
			val other:Range = o.asInstanceOf[Range]
			return this.begin == other.begin && this.end == other.end
		}
		return false
	}
}

// --- OFFSET ---
case class Offset(p:ReadablePeriod) {
	def +(diff:Offset):Offset = p.toPeriod.plus(diff)
	def -(diff:Offset):Offset = p.toPeriod.minus(diff)
	def apply(n:Int) = this.*(n)
	def *(n:Int):Offset = {
		if(p.isInstanceOf[Seconds]){
			p.asInstanceOf[Seconds].multipliedBy(n)
		} else if(p.isInstanceOf[Minutes]){
			p.asInstanceOf[Minutes].multipliedBy(n)
		} else if(p.isInstanceOf[Hours]){
			p.asInstanceOf[Hours].multipliedBy(n)
		} else if(p.isInstanceOf[Days]){
			p.asInstanceOf[Days].multipliedBy(n)
		} else if(p.isInstanceOf[Weeks]){
			p.asInstanceOf[Weeks].multipliedBy(n)
		} else if(p.isInstanceOf[Months]){
			p.asInstanceOf[Months].multipliedBy(n)
		} else if(p.isInstanceOf[Years]){
			p.asInstanceOf[Years].multipliedBy(n)
		} else {
			throw new IllegalStateException("Cannot multiply offset")
		}
	}
	def seconds = p.toPeriod.toStandardDuration.getStandardSeconds
	override def toString:String = {
		val rtn:String = p.toString
		rtn.substring(1,rtn.length)
	}
}

// --- TIME ---
case class Time(base:DateTime, offset:Offset) {
	def this(base:DateTime) = this(base, null)
	def this(offset:Offset) = this(null, offset)

	def isGrounded:Boolean = this.base != null
	def ground:Instant = {
		if(base == null){
			throw new IllegalStateException("Trying to ground ungrounded time")
		}
		if(offset == null){
			base.toInstant
		}else{
			base.plus(offset).toInstant
		}
	}

	def +(other:Time):Time = {
		if(other.base != null){ 
			throw new IllegalStateException("Adding two grounded dates")
		}
		this.+(other.offset)
	}
	def +(diff:Offset):Time = {
		if(base != null){
			new Time(base.plus(diff),offset)
		}else if(offset != null){
			new Time(base, offset+diff)
		}else{
			new Time(base, diff)
		}
	}
	def -(other:Time):Offset = {
		if(!this.isGrounded){
			throw new IllegalStateException("Subtracting from ungrounded time")
		}
		if(!other.isGrounded){
			throw new IllegalStateException("Subtracting an ungrounded time")
		}
		new Period(this.ground-other.ground)
	}
	def -(diff:Offset):Time = {
		if(base != null){
			new Time(base.minus(diff),offset)
		}else if(offset != null){
			new Time(base, offset-diff)
		}else{
			new Time(base, ZERO.minus(diff))
		}
	}
	def apply(ground:Time):Time = {
		if(base != null){
			throw new IllegalStateException("Time is already grounded!")
		}
		val newBase:DateTime = ground.base
		val newOffset:Offset = {
				if(offset == null && ground.offset == null){
					null
				} else if(offset == null){
					ground.offset
				} else if(ground.offset == null){
					offset
				} else {
					ground.offset
				}
			}
		if(newOffset != null && newBase != null){
			new Time(newBase.plus(newOffset))
		} else {
			new Time(newBase, newOffset)
		}
	}

	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[Time] || o.isInstanceOf[DateTime]){
			val other:Time = o.asInstanceOf[Time]
			if(this.isGrounded && other.isGrounded) {
				return this.ground.getMillis == other.ground.getMillis
			} else if(!this.isGrounded && !other.isGrounded) {
				return this.offset == other.offset
			}
		}
		return false
	}
	def ~(o:Any):Boolean = {
		if(o.isInstanceOf[Time] || o.isInstanceOf[DateTime]){
			val other:Time = o.asInstanceOf[Time]
			if(this.isGrounded && other.isGrounded) {
				return this.ground.getMillis == other.ground.getMillis
			} else if(!this.isGrounded && !other.isGrounded) {
				val thisOffset:Offset 
					= if(this.offset==null) Period.ZERO else this.offset
				val otherOffset:Offset 
					= if(other.offset==null) Period.ZERO else other.offset
				return (thisOffset-otherOffset).seconds == 0
			}
		}
		return false
	}

	override def toString:String = {
		{if(base == null) "x" else base.toString() } +
		{if(offset == null) "" else "+{" + offset + "}" }
	}
}

//------------------------------------------------------------------------------
// TIME OBJECTS
//------------------------------------------------------------------------------

object Lex {
	val NOW:Time = new Time(null,null)
	val ZERO:Period = Seconds.ZERO.toPeriod

	val SEC:Offset = Seconds.ONE
	val MIN:Offset = Minutes.ONE
	val HOUR:Offset = Hours.ONE
	val DAY:Offset = Days.ONE
	val WEEK:Offset = Weeks.ONE
	val MONTH:Offset = Months.ONE
	val YEAR:Offset = Years.ONE
}

object Offset {
	def apply(millis:Long):Offset = new Period(millis)
}

object Time {
	DateTimeZone.setDefault(DateTimeZone.UTC);

//	val nonNumericPart = """(D|W|M|Q|Y|SU|SP|FA|WT)"""
//	val date = """([0-9]{4})-?([0-9]{2})?-?([0-9]{2})?""".r
//	val yearpart = ("""([0-9]{4})-"""+nonNumericPart+"""([0-9]*)""").r
//	val duration = ("""P([0-9]+)"""+nonNumericPart).r

	def apply(t:String, timex:Timex) = {
//		try{
//			//--Case: Date
//			val date(year,month,day) = t
//		} catch{ case (e:MatchError) => { try{
//			//--Case: Date Imprecise
//			val yearpart(year,granularity,index) = t
//		} catch{ case (e:MatchError) => { try{
//			//--Case: Duration
//			val duration(range,granularity) = t
//		} catch{ case (e:MatchError) => {
//			println("FAILED: " + t)
//		}} }} }}
//		new Time
		null
	}

	def apply(year:Int, month:Int, day:Int, hour:Int, min:Int, sec:Int):Time = {
		new Time(new DateTime(year,month,day,hour,min,sec,0))
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

	def test = {
//		println(Time(2011,04,19))
//		println(NOW+DAY*2-WEEK)
//		println(NOW+DAY*5+WEEK)
//		println((NOW+DAY*5-WEEK)(Time(2011)))
//		println((NOW+DAY*5-WEEK+MILLIS(200))(Time(2011)))
//		println((NOW+DAY*5-WEEK+MILLIS(200))(NOW))
//		println((NOW+DAY*5-WEEK+MILLIS(200))(NOW))
//		println(Time(2011,04,19) - Time(2011,04,18,19))
		println(Time(2011,04,19) == Time(2011,04,19))
		println(Time(2011,04,19) == NOW)
		println((Range(Time(2011,04,19), NOW+DAY)>> DAY >| HOUR*5)(Time(2011,04,19)) )
	}
}
