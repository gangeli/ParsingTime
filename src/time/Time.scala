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
	implicit def period2Duration(p:ReadablePeriod):Duration = new Duration(p)
	implicit def Duration2period(o:Duration):ReadablePeriod = o.toPeriod
	implicit def instant2long(i:Instant):Long = i.getMillis
	implicit def long2instant(l:Long):Instant = new Instant(l)
	implicit def range2duration(r:Range):Duration = (r.end - r.begin)
}

class TimeException(s:String,e:Throwable) extends RuntimeException(s,e) {
	def this() = this("",null)
	def this(s:String) = this(s,null)
	def this(e:Throwable) = this(null,e)
}

//------------------------------------------------------------------------------
// TIME CLASSES
//------------------------------------------------------------------------------

// --- RANGE ---
case class Range(begin:Time, end:Time){
	def isGrounded:Boolean = begin.isGrounded && end.isGrounded
	def >>(diff:Duration) = new Range(begin+diff, end+diff)
	def <<(diff:Duration) = new Range(begin-diff, end-diff)
	def <|(diff:Duration) = new Range(begin-diff, begin)
	def |>(diff:Duration) = new Range(end, end+diff)
	def |<(diff:Duration) = new Range(begin, begin+diff)
	def >|(diff:Duration) = new Range(end-diff, end)
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

// --- Duration ---
class Duration(val p:ReadablePeriod,private val groundFn:Range=>Range){
	def this(p:ReadablePeriod) = this(p,(r:Range)=>r)
	def ground(r:Range):Range = {
		if(!r.isGrounded)
			{ throw new TimeException("Cannot ground to abstract time") }
		groundFn( r )
	}
	def ground(time:Time):Range = ground( Range(time, time+p) )
	def toPeriod:ReadablePeriod = p
	def +(diff:Duration):Duration 
		= new Duration(p.toPeriod.plus(diff),groundFn)
	def -(diff:Duration):Duration 
		= new Duration(p.toPeriod.minus(diff),groundFn)
	def apply(n:Int) = this.*(n)
	def *(n:Int):Duration = {
		if(p.isInstanceOf[Seconds]){
			new Duration(p.asInstanceOf[Seconds].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Minutes]){
			new Duration(p.asInstanceOf[Minutes].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Hours]){
			new Duration(p.asInstanceOf[Hours].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Days]){
			new Duration(p.asInstanceOf[Days].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Weeks]){
			new Duration(p.asInstanceOf[Weeks].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Months]){
			new Duration(p.asInstanceOf[Months].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Years]){
			new Duration(p.asInstanceOf[Years].multipliedBy(n),groundFn)
		} else {
			throw new IllegalStateException("Cannot multiply Duration")
		}
	}
	def seconds:Long = p.toPeriod.toStandardDuration.getStandardSeconds
	override def equals(o:Any):Boolean = {
		val other:Duration = if(o.isInstanceOf[ReadablePeriod]){
				new Duration(o.asInstanceOf[ReadablePeriod])
			} else if(o.isInstanceOf[Duration]){
				o.asInstanceOf[Duration]
			} else{
				null
			}
		if(other == null){ 
			return false;
		} else{
			return this.p == other.p && this.groundFn == other.groundFn
		}
	}
	def ~(o:Any):Boolean = {
		val other:Duration = if(o.isInstanceOf[ReadablePeriod]){
				new Duration(o.asInstanceOf[ReadablePeriod])
			} else if(o.isInstanceOf[Duration]){
				o.asInstanceOf[Duration]
			} else{
				null
			}
		if(other == null){ 
			return false;
		} else{
			return this.p == other.p
		}
	}
	override def toString:String = {
		val rtn:String = p.toString
		rtn.substring(1,rtn.length)
	}
}


// --- TIME ---
case class Time(base:DateTime, offset:Duration) {
	def this(base:DateTime) = this(base, null)
	def this(offset:Duration) = this(null, offset)

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
	def +(diff:Duration):Time = {
		if(base != null){
			new Time(base.plus(diff),offset)
		}else if(offset != null){
			new Time(base, offset+diff)
		}else{
			new Time(base, diff)
		}
	}
	def -(other:Time):Duration = {
		if(!this.isGrounded){
			throw new IllegalStateException("Subtracting from ungrounded time")
		}
		if(!other.isGrounded){
			throw new IllegalStateException("Subtracting an ungrounded time")
		}
		new Period(this.ground-other.ground)
	}
	def -(diff:Duration):Time = {
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
		val newOffset:Duration = {
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
				val thisOffset:Duration 
					= if(this.offset==null) Period.ZERO else this.offset
				val otherOffset:Duration 
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
	object LexUtil {
		def dow:(Range,Int)=>Range = (r:Range,i:Int) => {
			val begin:Time = 
				Time(r.begin.base.withDayOfWeek( ((i-1) % 7) + 1 ), null)
			Range(begin, begin+Days.ONE)
		}
		def dom:(Range,Int)=>Range = (r:Range,i:Int) => {
			val begin:Time = 
				Time(r.begin.base.withDayOfMonth( ((i-1) % 31) + 1 ), null)
			Range(begin, begin+Days.ONE)
		}
		def doy:(Range,Int)=>Range = (r:Range,i:Int) => {
			val begin:Time = 
				Time(r.begin.base.withDayOfYear( ((i-1) % 7) + 366 ), null)
			Range(begin, begin+Days.ONE)
		}
		def woy:(Range,Int)=>Range = (r:Range,i:Int) => {
			val begin:Time = 
				Time(r.begin.base.withWeekOfWeekyear( ((i-1) % 53) + 1 ), null)
			Range(begin, begin+Weeks.ONE)
		}
		def moy:(Range,Int)=>Range = (r:Range,i:Int) => {
			val begin:Time = 
				Time(r.begin.base.withMonthOfYear( ((i-1) % 7) + 12 ), null)
			Range(begin, begin+Months.ONE)
		}
	}
	//--Durations
	val SEC:Duration = Seconds.ONE
	val MIN:Duration = Minutes.ONE
	val HOUR:Duration = Hours.ONE
	val DAY:Duration = Days.ONE
	val WEEK:Duration = Weeks.ONE
	val MONTH:Duration = Months.ONE
	val YEAR:Duration = Years.ONE
	//--Misc
	val NOW:Time = new Time(null,null)
	val ZERO:Period = Seconds.ZERO.toPeriod
	val NOTIME:Range = Range(NOW,NOW+DAY)
	val NODUR:Duration = DAY
	//--Day of Week
	val MON:Duration = new Duration(Weeks.ONE, LexUtil.dow(_,1))
	val TUE:Duration = new Duration(Weeks.ONE, LexUtil.dow(_,2))
	val WED:Duration = new Duration(Weeks.ONE, LexUtil.dow(_,3))
	val THU:Duration = new Duration(Weeks.ONE, LexUtil.dow(_,4))
	val FRI:Duration = new Duration(Weeks.ONE, LexUtil.dow(_,5))
	val SAT:Duration = new Duration(Weeks.ONE, LexUtil.dow(_,6))
	val SUN:Duration = new Duration(Weeks.ONE, LexUtil.dow(_,7))
	//--OTHER DURATIONS
	def DOM(i:Int) = new Duration(Months.ONE, LexUtil.dom(_,i))
	
	//--Shifts
	val catLeft:(Range,Duration)=>Range = _ <| _
	val catRight:(Range,Duration)=>Range = _ |> _
	val shrinkBegin:(Range,Duration)=>Range = _ |< _
	val shrinkEnd:(Range,Duration)=>Range = _ >| _
}

object Duration {
	def apply(millis:Long):Duration = new Period(millis)
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
//		println(Time(2011,04,19) == Time(2011,04,19))
//		println(Time(2011,04,19) == NOW)
//		println((Range(Time(2011,04,19), NOW+DAY)>> DAY >| HOUR*5)(Time(2011,04,19)) )
		println(MON)
	}
}
