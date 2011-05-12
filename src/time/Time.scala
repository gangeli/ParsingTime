package time

import Conversions._
import Lex._
import org.joda.time._

//------------------------------------------------------------------------------
// UTILS
//------------------------------------------------------------------------------
trait PartialParse {def accepts(s:Symbol):Boolean; def typeTag:Symbol}
object Conversions {
	implicit def period2Duration(p:ReadablePeriod):Duration = new Duration(p)
	implicit def Duration2period(o:Duration):ReadablePeriod = o.toPeriod
	implicit def instant2long(i:Instant):Long = i.getMillis
	implicit def long2instant(l:Long):Instant = new Instant(l)
	implicit def range2duration(r:Range):Duration = (r.end - r.begin)

	implicit def rangePairFxn2PP(fn:(Range,Range)=>Range):PartialParse
		= new PartialParse { 
				def apply(r:Range) = fn(r,_:Range)
				override def accepts(s:Symbol):Boolean = s == 'Range
				override def typeTag:Symbol = 'FunctionRangeRange
				override def toString:String = "<<rangeRangeFunction>>"
			}
	implicit def rangeDurationFxn2PP(fn:(Range,Duration)=>Range):PartialParse
		= new PartialParse { 
				def apply(r:Range) = fn(r,_:Duration)
				override def accepts(s:Symbol):Boolean = s == 'Range
				override def typeTag:Symbol = 'FunctionRangeDuration
				override def toString:String = "<<rangeDurationFunction>>"
			}
	implicit def rangeFxn2PP(fn:Range=>Range):PartialParse
		= new PartialParse{ 
				def apply(r:Range) = fn(r)
				override def accepts(s:Symbol):Boolean = s == 'Range
				override def typeTag:Symbol = 'FunctionRange
				override def toString:String = "<<rangeFunction>>"
			}
	implicit def durationFxn2PP(fn:Duration=>Range):PartialParse
		= new PartialParse{
				def apply(d:Duration) = fn(d)
				override def accepts(s:Symbol):Boolean = s == 'Duration
				override def typeTag:Symbol = 'FunctionDuration
				override def toString:String = "<<durationFunction>>"
			}
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
case class Range(begin:Time, end:Time) extends PartialParse{
	def isGrounded:Boolean = begin.isGrounded && end.isGrounded

	def ::(ground:Time=>Time) = new Range(ground :: begin, ground :: end)
	def >>(diff:Duration) = new Range(begin+diff, end+diff)
	def <<(diff:Duration) = new Range(begin-diff, end-diff)
	def <|(diff:Duration) = new Range(begin-diff, begin)
	def |>(diff:Duration) = new Range(end, end+diff)
	def |<(diff:Duration) = new Range(begin, begin+diff)
	def >|(diff:Duration) = new Range(end-diff, end)
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
			val newEnd = newBegin+smaller.norm
			//(return)
			Range(newBegin, newEnd)
		}
	}
	def cons(other:Range):Range = Range(this.begin, other.end)
	def apply(ground:Time):Range = {
		new Range(
				{if(begin.isGrounded) begin else begin(ground)},
				{if(end.isGrounded) end else end(ground)}
			)
	}
	def accepts(s:Symbol):Boolean = {
		if((!begin.isGrounded || !end.isGrounded) && s == 'Time) true else false
	}
	def typeTag:Symbol = 'Range

	def norm:Duration = {
		if(begin.offset == null){
			if(end.offset == null){
				//(case: zero)
				Seconds.ZERO.toPeriod
			}else{
				//(case: second offset)
				end.offset
			}
		} else {
			if(end.offset == null){
				//(case: first offset)
				Seconds.ZERO.toPeriod - begin.offset
			}else{
				//(case: both offset)
				end-begin //TODO more elegant norm
			}
		}
	}
	def ~(o:Any):Boolean = {
		if(o.isInstanceOf[Range]){
			val other:Range = o.asInstanceOf[Range]
			return (this.begin ~ other.begin) && (this.end ~ other.end)
		}
		return false
	}
	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[Range]){
			val other:Range = o.asInstanceOf[Range]
			return this.begin == other.begin && this.end == other.end
		}
		return false
	}
	override def toString:String 
		= "(" + begin + " , " + end+")"
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}

// --- Duration ---
class Duration(val p:ReadablePeriod,private val groundFn:Time=>Range) 
		extends PartialParse{
	def this(p:ReadablePeriod) = this(p,null)
	def isGroundable = groundFn != null
	def grounding:Time=>Time 
		= if(isGroundable) (tm:Time) => groundFn(tm).begin else null
	def flatten:Duration = new Duration(p,null)
	def apply(t:Time):Range = {
		if(groundFn == null){ 
			Range(t, t+p)
		} else {
			if(t.isGrounded){
				groundFn(t)
			} else {
				//(ground)
				val fn = grounding
				val grounded 
					= fn :: Range(new Time(null,null,null), new Time(null,null,null))
				//(apply offset)
				assert(!groundFn(t).begin.isGrounded, "bad groundFn (grounded)")
				assert(groundFn(t).begin.offset == null, "bad groundFn (offset)")
				Range(grounded.begin, grounded.end + groundFn(t).end.offset)
			}
		}
	}
	def accepts(s:Symbol):Boolean = s == 'Time
	def typeTag:Symbol = 'Duration
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
		val yearContrib:Long = period.getYears.longValue*365*24*60*60
		period = period.withMonths(0).withYears(0)
		period.toStandardDuration.getStandardSeconds+monthContrib+yearContrib
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
			return this.p.seconds == other.p.seconds
		}
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
	override def toString:String = {
		val rtn:String = p.toString
		rtn.substring(1,rtn.length)
	}
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}


// --- TIME ---
case class Time(base:DateTime, offset:Duration, modifiers:List[Time=>Time]) 
		extends PartialParse{
	def this(base:DateTime,offset:Duration) = this(base, offset, null)
	def this(base:DateTime) = this(base, null, null)
	def this(offset:Duration) = this(null, offset, null)

	def isGrounded:Boolean = this.base != null
	def alsoMod(mods:List[Time=>Time]):Time = {
		val existingMods = if(modifiers == null) List() else modifiers
		if(offset == null){
			new Time(base, null, mods ::: existingMods)
		} else {
			new Time(base, null, mods ::: (((t:Time)=>t+offset) :: existingMods) )
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

	def ::(ground:Time=>Time):Time = {
		if(ground == null){ return this }
		val existingMods = if(modifiers == null) List() else modifiers
		if(offset == null){
			new Time(base, offset, ground :: existingMods)
		} else {
			new Time(base, null, ground :: (((t:Time)=>t+offset) :: existingMods) )
		}
	}
	def +(diff:Duration):Time = {
		val added = if(isGrounded){
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
		diff.grounding :: added
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
			val millis:Long = this.ground.getMillis - other.ground.getMillis
			if(millis > Integer.MAX_VALUE){
				Duration.INFINITE
			} else if(millis < Integer.MIN_VALUE) {
				Duration.NEG_INFINITE
			} else {
				new Period(millis)
			}
		} else {
			//(case: approximating subtraction)
			val thisSec = if(this.offset == null) 0 else this.offset.seconds
			val thatSec = if(other.offset == null) 0 else other.offset.seconds
			Seconds.seconds((thisSec - thatSec).asInstanceOf[Int])
		}
	}
	def -(diff:Duration):Time = {
		val subtracted = if(isGrounded){
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
		diff.grounding :: subtracted
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
	def accepts(s:Symbol):Boolean = {
		!isGrounded && s == 'Time
	}
	def typeTag:Symbol = 'Time

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
					Time.probablyEqual(this.modifiers,other.modifiers)
			}
		}
		return false
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
	override def toString:String = {
		{if(base == null) "x" else base.toString() } +
		{if(offset == null) "" else "+{" + offset + "}" } +
		{if(modifiers == null) "" else "|"+modifiers.length+"mod"}
	}
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}

//------------------------------------------------------------------------------
// TIME OBJECTS
//------------------------------------------------------------------------------

object Lex {
	object LexUtil {
		def dow:(Time,Int)=>Range = (t:Time,iArg:Int) => {
			val i:Int = if(iArg < 0) t.base.getDayOfWeek else iArg
			val begin:Time = if(t.isGrounded){
					Time(t.base.withDayOfWeek(i).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Days.ONE)
		}
		def dom:(Time,Int)=>Range = (t:Time,iArg:Int) => {
			val i:Int = if(iArg < 0) t.base.getDayOfMonth else iArg
			val begin:Time = if(t.isGrounded){
					Time(t.base.withDayOfMonth(i).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Days.ONE)
		}
		def woy:(Time,Int)=>Range = (t:Time,iArg:Int) => {
			val i:Int = if(iArg < 0) t.base.getWeekOfWeekyear else iArg
			val begin:Time = if(t.isGrounded){
					Time(t.base.withWeekOfWeekyear(i)
						.withDayOfWeek(1).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Weeks.ONE)
		}
		def moy:(Time,Int)=>Range = (t:Time,iArg:Int) => {
			val i:Int = if(iArg < 0) t.base.getMonthOfYear else iArg
			val begin:Time = if(t.isGrounded){
					Time(t.base.withMonthOfYear(i)
						.withDayOfMonth(1).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Months.ONE)
		}
		def qoy:(Time,Int)=>Range = (t:Time,iArg:Int) => {
			val i:Int = if(iArg < 0) ((t.base.getMonthOfYear-1)%3)+1 else iArg
			val begin:Time = if(t.isGrounded){
					Time(t.base.withMonthOfYear(3*(i-1)+1)
						.withDayOfMonth(1).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Months.THREE)
		}
	}
	//--Durations
	val SEC:Duration = Seconds.ONE
	val MIN:Duration = Minutes.ONE
	val HOUR:Duration = Hours.ONE
	val DAY:Duration = Days.ONE
	val WEEK:Duration = Weeks.ONE
	val MONTH:Duration = Months.ONE
	val QUARTER:Duration = Months.THREE
	val YEAR:Duration = Years.ONE
	//--Misc
	val ZERO:Period = Seconds.ZERO.toPeriod
	val NOW:Time = new Time(null,null)
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
	def DOW(i:Int) = new Duration(Weeks.ONE, LexUtil.dow(_,i))
	def DOM(i:Int) = new Duration(Months.ONE, LexUtil.dom(_,i))
	def WOY(i:Int) = new Duration(Years.ONE, LexUtil.woy(_,i))
	def MOY(i:Int) = new Duration(Years.ONE, LexUtil.moy(_,i))
	def QOY(i:Int) = new Duration(Years.ONE, LexUtil.qoy(_,i))
	def YEAR(i:Int) = new Range(Time(i),Time(i+1))
	val AYEAR = new Duration(Years.ONE, (t:Time) => {
			val begin:Time = if(t.isGrounded){
					Time(t.base.withDayOfYear(1).withMillisOfDay(0), null)
				} else {
					new Time(null,null)
				}
			Range(begin, begin+Years.ONE)
		})
	
	//--Shifts
	val shiftLeft:(Range,Duration)=>Range = _ << _
	val shiftRight:(Range,Duration)=>Range = _ >> _
	val catLeft:(Range,Duration)=>Range = _ <| _
	val catRight:(Range,Duration)=>Range = _ |> _
	val shrinkBegin:(Range,Duration)=>Range = _ |< _
	val shrinkEnd:(Range,Duration)=>Range = _ >| _
	val intersect:(Range,Range)=>Range = _ ^ _
	val cons:(Range,Range)=>Range = _.cons(_)
}

object Range {
}

object Duration {
	def apply(millis:Long):Duration = new Period(millis)
	val INFINITE:Duration = Period.years(Integer.MAX_VALUE)
	val NEG_INFINITE:Duration = Period.years(Integer.MIN_VALUE)
}

object Time {
	val DAWN_OF = new Time(new DateTime(java.lang.Long.MIN_VALUE), null, null)
	val END_OF = new Time(new DateTime(java.lang.Long.MAX_VALUE), null, null)

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

	def probablyEqual(a:Time=>Time, b:Time=>Time):Boolean = {
		val preDaylightSavings = Time(2011,3,12,4,26)
		val postDaylightSavings = Time(2011,3,13,9,12)
		val random = Time(1985,10,4)
		(a(preDaylightSavings) ~ b(preDaylightSavings)) &&
			(a(postDaylightSavings) ~ b(postDaylightSavings)) &&
			(a(random) ~ b(random))
	}
	def probablyEqual(a:List[Time=>Time], b:List[Time=>Time]):Boolean = {
		if(a == null && b == null){ return true }
		if(a != null && b != null){
		if(a.length != b.length){ return false }
			a.zip(b).forall{ case (ai,bi) => probablyEqual(ai,bi) }
		} else {
			false
		}
	}
	def probablyEqualRange(a:Range=>Range, b:Range=>Range):Boolean = {
		import scala.util.Random
		val r:Random = new Random(42)
		for( i <- 1 until 100 ) {
			val arg:Range = Range(
				Time(r.nextInt(3000),r.nextInt(11)+1,r.nextInt(27)+1,
					r.nextInt(24),r.nextInt(60),r.nextInt(60)),
				Time(r.nextInt(3000),r.nextInt(11)+1,r.nextInt(27)+1,
					r.nextInt(24),r.nextInt(60),r.nextInt(60)))
			if( !(a(arg) ~ b(arg)) ){
				return false
			}
		}
		return true
	}

	var interpreter:scala.tools.nsc.Interpreter = null
	def interactive = {
		import scala.tools.nsc.{Interpreter,Settings}
		//--Create Interpreter
		println("Loading interpreter...")
		if(interpreter == null){
			//(objects)
			val settings = new Settings
			settings.usejavacp.value = true
			interpreter = new Interpreter(settings)
			//(initialize)
			interpreter.interpret("import time._")
			interpreter.interpret("import time.Lex._")
			interpreter.interpret("import time.Conversions._")
			interpreter.interpret("val ground = Time(2011,4,26)")
		}
		//--Loop
		var cond = true
		while(cond){
			val str = Console.readLine("scala> ")
			if(str.trim.equalsIgnoreCase("exit")){
				cond = false
			} else {
				interpreter.interpret(str)
			}
		}
	}

	def test = {
		println
		println("     -------START------")

		val tomorrow = catRight(NOTIME,NODUR)
		val until = cons
		val target = (r:Range) => Range(r.begin,NOW+DAY)
		val ground = Range(Time(2011,4,25),Time(2011,4,26))
		println(target(ground))
		val guess = until(_:Range,tomorrow)
		println(guess(ground))
		assert( Time.probablyEqualRange(target,until(_:Range,tomorrow)) )
		
		println
	}
}
