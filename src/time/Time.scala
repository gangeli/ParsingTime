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
case class Range(begin:Time, end:Time, groundFn:Time=>Time){
	def this(begin:Time, end:Time) = this(begin, end, null)
	def preGround(fn:Time=>Time):Range = new Range(begin, end, fn)
	def isGrounded:Boolean = begin.isGrounded && end.isGrounded
	private def createFn(diff:Duration):Time=>Time = {
		if(groundFn == null && !diff.isGroundable){
			null
		} else if(groundFn == null){
			diff(_).begin
		} else if(diff == null){ 
			groundFn
		} else {
			(t:Time) => diff(groundFn(t)).begin
		}
	}

	def >>(diff:Duration) = new Range(begin+diff, end+diff, createFn(diff))
	def <<(diff:Duration) = new Range(begin-diff, end-diff, createFn(diff))
	def <|(diff:Duration) = new Range(begin-diff, begin, createFn(diff))
	def |>(diff:Duration) = new Range(end, end+diff, createFn(diff))
	def |<(diff:Duration) = new Range(begin, begin+diff, createFn(diff))
	def >|(diff:Duration) = new Range(end-diff, end, createFn(diff))
	def ^(r:Range):Range = {
		if( (!isGrounded && r.isGrounded) || (isGrounded && !r.isGrounded) ){
			//--Case: grounded and abstract
			val grounded = if(isGrounded) this else r
			val abstr = if(isGrounded) r else this
			abstr(grounded.begin)
		} else if(isGrounded){
			//--Case: grounded and grounded
			val begin 
				=if(this.begin.base.compareTo(r.begin.base) > 0) this.begin else r.begin
			val end 
				=if(this.end.base.compareTo(r.end.base) < 0) this.end else r.end
			Range(begin,end)
		} else {
			//--Case: abstract and abstract
			//(identify larger and smaller)
			val larger = if(this.norm > r.norm) this else r
			val smaller = if(this.norm > r.norm) r else this
			//(take begin from the larger)
			val newBegin = larger.begin.alsoMod(smaller.begin)
			//(take end from larger.begin+smaller.norm)
			val newEnd = larger.begin+smaller.norm
			//(return)
			Range(newBegin, newEnd)
		}
	}
	def apply(grnd:Time):Range = {
		val ground:Time = if(groundFn == null) grnd else groundFn(grnd)
		new Range(
				{if(!begin.isGrounded) begin(ground) else begin},
				{if(!end.isGrounded) end(ground) else end}
			)
	}

	def norm:Duration = end-begin
	def ~(o:Any):Boolean = {
		if(o.isInstanceOf[Range]){
			val other:Range = o.asInstanceOf[Range]
			if(this.groundFn == null && other.groundFn != null ||
					this.groundFn != null && other.groundFn == null){
				return false
			}
			return (this.begin ~ other.begin) && (this.end ~ other.end) &&
				(	(this.groundFn == null && other.groundFn == null) || 
					(groundFn(Time(1970,1,1)) ~ other.groundFn(Time(1970,1,1))) )
		}
		return false
	}
	override def toString:String 
		= "(" + begin + ", " + end + {if(groundFn != null) " | <cond>" else "" }+")"
	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[Range]){
			val other:Range = o.asInstanceOf[Range]
			return this.begin == other.begin && this.end == other.end &&
				this.groundFn == other.groundFn
		}
		return false
	}
}

// --- Duration ---
class Duration(val p:ReadablePeriod,private val groundFn:Time=>Range){
	def this(p:ReadablePeriod) = this(p,null)
	def isGroundable = groundFn != null
	def apply(t:Time):Range = {
		if(groundFn == null){ 
			Range(t, t+p)
		} else {
			if(t.isGrounded){
				groundFn(t)
			} else {
				groundFn(t).preGround( (tm:Time) => groundFn(tm).begin )
			}
		}
	}
	def toPeriod:ReadablePeriod = p
	def +(diff:Duration):Duration 
		= new Duration(p.toPeriod.plus(diff),groundFn)
	def -(diff:Duration):Duration 
		= new Duration(p.toPeriod.minus(diff),groundFn)
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
	def seconds:Long = {
		var period = p.toPeriod
		val monthContrib:Long = period.getMonths*30*24*60*60
		val yearContrib:Long = period.getYears*365*24*60*60
		period = period.withMonths(0).withYears(0)
		period.toStandardDuration.getStandardSeconds+monthContrib+yearContrib
	}
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
	def <(other:Duration) = this.seconds < other.seconds
	def >(other:Duration) = this.seconds > other.seconds
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
case class Time(base:DateTime, offset:Duration, modifiers:List[Time=>Time]) {
	def this(base:DateTime,offset:Duration) = this(base, offset, null)
	def this(base:DateTime) = this(base, null, null)
	def this(offset:Duration) = this(null, offset, null)

	def isGrounded:Boolean = this.base != null
	def alsoMod(mods:List[Time=>Time]):Time = {
		if(mods == null){
			this
		} else {
			new Time(base, offset, mods ::: modifiers)
		}
	}
	def alsoMod(other:Time):Time = alsoMod(other.modifiers)

	def ground:Instant = {
		if(!isGrounded){
			throw new TimeException("Trying to ground ungrounded time")
		}else{
			var rtn:DateTime = base
			//(apply modifiers)
			if(modifiers != null){
				modifiers.reverse.foreach( (mod:Time=>Time) => {
					val cand = mod(new Time(rtn))
					assert(cand.offset == null, "offset after mod should be null")
					assert(cand.modifiers == null, "modifiers after mod should be null")
					rtn = cand.base
				})
			}
			//(apply offset)
			if(offset != null){
				rtn = rtn.plus(offset)
			}
			//(return)
			rtn.toInstant
		}
	}

//	def +(other:Time):Time = {
//		if(other.isGrounded){ 
//			throw new IllegalStateException("Adding two grounded dates")
//		}
//		this.+(other.offset)
//	}
	def +(diff:Duration):Time = {
		if(isGrounded){
			//(case: adding to grounded time)
			assert(offset == null, "Offset should be null")
			new Time(base.plus(diff),offset,modifiers)
		}else if(offset != null){
			//(case: adding to existing offset)
			new Time(base, offset+diff, modifiers)
		}else{
			//(case: adding new offset)
			new Time(base, diff, modifiers)
		}
	}
	def -(other:Time):Duration = {
		//(error checks)
		if( (!this.isGrounded && other.isGrounded) ||
				(this.isGrounded && !other.isGrounded) ){
			throw new IllegalStateException(
				"Cannot subtract grounded and ungrounded times");
		}
		if(this.isGrounded){
			//(case: subtracting grounded times)
			new Period(this.ground-other.ground)
		} else {
			//(case: approximating subtraction)
			val thisSec = if(this.offset == null) 0 else this.offset.seconds
			val thatSec = if(other.offset == null) 0 else other.offset.seconds
			Seconds.seconds((thisSec - thatSec).asInstanceOf[Int])
		}
	}
	def -(diff:Duration):Time = {
		if(isGrounded){
			//(case: subtracting grounded times)
			assert(offset == null, "Offset should be null for grounded time")
			new Time(base.minus(diff),offset,modifiers)
		}else if(offset != null){
			//(case: existing offset)
			new Time(base, offset-diff, modifiers)
		}else{
			//(case: no offset)
			new Time(base, ZERO.minus(diff), modifiers)
		}
	}
	def apply(grnd:Time):Time = {
		var ground = grnd
		//--Argument check
		if(isGrounded){ throw new TimeException("Time is already grounded!") }
		if(!ground.isGrounded){ throw new TimeException("Argument not grounded!") }
		assert(ground.offset == null, "Ground should not have an offset")
		assert(ground.modifiers == null, "Ground should not have modifiers")
		//--Modifiers
		if(modifiers != null){
			modifiers.reverse.foreach( (mod:Time=>Time) => {
				ground = mod(ground)
			})
		}
		//--Offset
		if(offset != null){
			ground = ground + offset
		}
		//--Return
		new Time(ground.base, null, null)
	}

	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[Time] || o.isInstanceOf[DateTime]){
			val other:Time = o.asInstanceOf[Time]
			if(this.isGrounded && other.isGrounded) {
				return this.ground.getMillis == other.ground.getMillis
			} else if(!this.isGrounded && !other.isGrounded) {
				return this.offset == other.offset && this.modifiers == other.modifiers
			}
		}
		return false
	}
	def ~(o:Any):Boolean = {
		if(o.isInstanceOf[Time] || o.isInstanceOf[DateTime]){
			val other:Time = o.asInstanceOf[Time]
			if(this.isGrounded && other.isGrounded) {
				//(case: both grounded)
				return this.ground.getMillis == other.ground.getMillis
			} else if(!this.isGrounded && !other.isGrounded) {
				//(case: both ungrounded)
				val thisOffset:Duration 
					= if(this.offset==null) Period.ZERO else this.offset
				val otherOffset:Duration 
					= if(other.offset==null) Period.ZERO else other.offset
				return (thisOffset-otherOffset).seconds == 0 &&
					this.modifiers == other.modifiers
			}
		}
		return false
	}

	override def toString:String = {
		{if(base == null) "x" else base.toString() } +
		{if(offset == null) "" else "+{" + offset + "}" } +
		{if(modifiers == null) "" else "|"+modifiers.length+"mod"}
	}
}

//------------------------------------------------------------------------------
// TIME OBJECTS
//------------------------------------------------------------------------------

object Lex {
	object LexUtil {
		def dow:(Time,Int)=>Range = (t:Time,i:Int) => {
			val begin:Time = if(t.isGrounded){
					Time(t.base.withDayOfWeek(i).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Days.ONE)
		}
		def dom:(Time,Int)=>Range = (t:Time,i:Int) => {
			val begin:Time = if(t.isGrounded){
					Time(t.base.withDayOfMonth(i).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Days.ONE)
		}
		def woy:(Time,Int)=>Range = (t:Time,i:Int) => {
			val begin:Time = if(t.isGrounded){
					Time(t.base.withWeekOfWeekyear(i)
						.withDayOfWeek(1).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Weeks.ONE)
		}
		def moy:(Time,Int)=>Range = (t:Time,i:Int) => {
			val begin:Time = if(t.isGrounded){
					Time(t.base.withMonthOfYear(i)
						.withDayOfMonth(1).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Months.ONE)
		}
//		def dom:(Range,Int)=>Range = (r:Range,i:Int) => {
//			val begin:Time = 
//				Time(r.begin.base.withDayOfMonth( ((i-1) % 31) + 1 ), null)
//			Range(begin, begin+Days.ONE)
//		}
//		def doy:(Range,Int)=>Range = (r:Range,i:Int) => {
//			val begin:Time = 
//				Time(r.begin.base.withDayOfYear( ((i-1) % 7) + 366 ), null)
//			Range(begin, begin+Days.ONE)
//		}
//		def woy:(Range,Int)=>Range = (r:Range,i:Int) => {
//			val begin:Time = 
//				Time(r.begin.base.withWeekOfWeekyear( ((i-1) % 53) + 1 ), null)
//			Range(begin, begin+Weeks.ONE)
//		}
//		def moy:(Range,Int)=>Range = (r:Range,i:Int) => {
//			val begin:Time = 
//				Time(r.begin.base.withMonthOfYear( ((i-1) % 7) + 12 ), null)
//			Range(begin, begin+Months.ONE)
//		}
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
	def MOY(i:Int) = new Duration(Years.ONE, LexUtil.moy(_,i))
	val AYEAR = new Duration(Years.ONE, (t:Time) => {
			val begin:Time = if(t.isGrounded){
					Time(t.base.withDayOfYear(1).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Years.ONE)
		})
	
	//--Shifts
	val catLeft:(Range,Duration)=>Range = _ <| _
	val catRight:(Range,Duration)=>Range = _ |> _
	val shrinkBegin:(Range,Duration)=>Range = _ |< _
	val shrinkEnd:(Range,Duration)=>Range = _ >| _
	val intersect:(Range,Range)=>Range = _ ^ _
}

object Range {
	def apply(begin:Time, end:Time):Range = apply(begin, end, null)
}

object Duration {
	def apply(millis:Long):Duration = new Period(millis)
}

object Time {
	DateTimeZone.setDefault(DateTimeZone.UTC);
	def apply(base:DateTime, offset:Duration) = new Time(base, offset, null)

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
		println
		println("     -------START------")
		val march = (t:Time) => 
			new Time(t.base.withMonthOfYear(3).withDayOfMonth(1).withMillisOfDay(0))
		val fri = (t:Time) => 
			new Time(t.base.withDayOfWeek(5).withMillisOfDay(0))
		val time = new Time(null, DAY, List(fri,march))
		println( time(Time(2011,04,30)) )
	}
}
