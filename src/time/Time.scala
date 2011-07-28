package time

import Conversions._
import Lex._
import org.joda.time._

//------------------------------------------------------------------------------
// UTILS
//------------------------------------------------------------------------------
object Conversions {
	implicit def period2Duration2(p:ReadablePeriod):Duration2 = new Duration2(p)
	implicit def Duration22period(o:Duration2):ReadablePeriod = o.toPeriod
	implicit def instant2long(i:Instant):Long = i.getMillis
	implicit def long2instant(l:Long):Instant = new Instant(l)
	implicit def Range22Duration2(r:Range2):Duration2 = (r.end - r.begin)
}


//------------------------------------------------------------------------------
// Time2 CLASSES
//------------------------------------------------------------------------------


// --- Range2 ---
case class Range2(begin:Time2, end:Time2) {
	def isGrounded:Boolean = begin.isGrounded && end.isGrounded

	def ::(ground:Time2=>Time2) = new Range2(ground :: begin, ground :: end)
	def >>(diff:Duration2) = new Range2(begin+diff, end+diff)
	def <<(diff:Duration2) = new Range2(begin-diff, end-diff)
	def <|(diff:Duration2) = new Range2(begin-diff, begin)
	def |>(diff:Duration2) = new Range2(end, end+diff)
	def |<(diff:Duration2) = new Range2(begin, begin+diff)
	def >|(diff:Duration2) = new Range2(end-diff, end)
	def ^(r:Range2):Range2 = {
		def mkBegin(a:Time2,b:Time2)
			= if(a.base.compareTo(b.base) > 0) a else b
		def mkEnd(a:Time2,b:Time2)
			= if(a.base.compareTo(b.base) < 0) a else b
		if(this.isGrounded && r.isGrounded){
			//--Case: grounded and grounded
			Range2(mkBegin(this.begin,r.begin),mkEnd(this.end,r.end))
		} else if(!this.isGrounded && !r.isGrounded){
			//--Case: abstract and abstract
			//(grounded begin Time2)
			val begin:Time2 = Time2(null,null,List[Time2=>Time2]( (ground:Time2) => {
					assert(ground.isGrounded, "Modifying with ungrounded Time2")
					mkBegin(this.begin(ground),r.begin(ground))
				}))
			//(grounded end Time2)
			val end:Time2 = Time2(null,null,List[Time2=>Time2]( (ground:Time2) => {
					assert(ground.isGrounded, "Modifying with ungrounded Time2")
					mkEnd(this.end(ground),r.end(ground))
				}))
			//(make Range2)
			Range2(begin,end)
		} else {
			//--Case: grounded and abstract
			//(find grounded and ungrounded)
			val grounded = if(isGrounded) this else r
			val abstr = if(isGrounded) r else this
			val groundedAbstr:Range2 = abstr(grounded.begin)
			//(make Range2)
			val begin:Time2 = mkBegin(groundedAbstr.begin, grounded.begin)
			val end:Time2 = mkEnd(groundedAbstr.end, grounded.end)
			//(make Range2)
			Range2(begin,end)
		}
	}
	def cons(other:Range2):Range2 = Range2(this.begin, other.end)
	def apply(ground:Time2):Range2 = {
		assert(ground.isGrounded, "grounding Range2 with ungrounded Time2")
		assert(ground.offset == null, "grounded Time2 has an offset: "+ground.offset)
		assert(ground.modifiers == null, 
			"grounded Time2 has modifiers: "+ground.modifiers)
		new Range2(
				{if(begin.isGrounded) begin else begin(ground)},
				{if(end.isGrounded) end else end(ground)}
			)
	}
	def accepts(s:Symbol):Boolean = {
		if((!begin.isGrounded || !end.isGrounded) && s == 'Time2) true else false
	}
	def typeTag:Symbol = 'Range2

	def norm:Duration2 = {
		if(begin.isGrounded){
			if(end.isGrounded){
				//(case: both grounded)
				end-begin
			} else {
				//(case: begin grounded only)
				Duration2.INFINITE
			}
		} else {
			if(end.isGrounded){
				//(case: end grounded only)
				Duration2.INFINITE
			} else if(begin.offset == null){
				if(end.offset == null){
					//(case: ungrounded, no offsets)
					Duration2.ZERO
				} else {
					//(case: ungrounded, end offset)
					end.offset
				}
			} else {
				if(end.offset == null){
					//(case: ungrounded, begin offset)
					Duration2.ZERO - begin.offset
				} else {
					//(case: ungrounded, both offset)
					end.offset - begin.offset
				}
			}
		}
	}
	def ~(o:Any):Boolean = {
		if(o.isInstanceOf[Range2]){
			val other:Range2 = o.asInstanceOf[Range2]
			return (this.begin ~ other.begin) && (this.end ~ other.end)
		}
		return false
	}
	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[Range2]){
			val other:Range2 = o.asInstanceOf[Range2]
			return this.begin == other.begin && this.end == other.end
		}
		return false
	}
	override def toString:String 
		= "(" + begin + " , " + end+")"
	override def hashCode:Int =throw new IllegalStateException("Dont hash me bro")
}

// --- Duration2 ---
class Duration2(val p:ReadablePeriod,private val groundFn:Time2=>Range2) {
	def this(p:ReadablePeriod) = this(p,null)
	def isGroundable = groundFn != null
	def grounding:Time2=>Time2 
		= if(isGroundable) (tm:Time2) => groundFn(tm).begin else null
	def flatten:Duration2 = new Duration2(p,null)
	def apply(t:Time2):Range2 = {
		if(groundFn == null){ 
			Range2(t, t+p)
		} else {
			if(t.isGrounded){
				groundFn(t)
			} else {
				//(ground)
				val fn = grounding
				val grounded 
					= fn :: Range2(new Time2(null,null,null), new Time2(null,null,null))
				//(apply offset)
				assert(!groundFn(t).begin.isGrounded, "bad groundFn (grounded)")
				assert(groundFn(t).begin.offset == null, "bad groundFn (offset)")
				Range2(grounded.begin, grounded.end + groundFn(t).end.offset)
			}
		}
	}
	def accepts(s:Symbol):Boolean = s == 'Time2
	def typeTag:Symbol = 'Duration2
	def toPeriod:ReadablePeriod = p
	def +(diff:Duration2):Duration2 
		= new Duration2(p.toPeriod.plus(diff),groundFn)
	def -(diff:Duration2):Duration2 
		= new Duration2(p.toPeriod.minus(diff),groundFn)
	def *(n:Int):Duration2 = {
		if(p.isInstanceOf[Seconds]){
			new Duration2(p.asInstanceOf[Seconds].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Minutes]){
			new Duration2(p.asInstanceOf[Minutes].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Hours]){
			new Duration2(p.asInstanceOf[Hours].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Days]){
			new Duration2(p.asInstanceOf[Days].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Weeks]){
			new Duration2(p.asInstanceOf[Weeks].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Months]){
			new Duration2(p.asInstanceOf[Months].multipliedBy(n),groundFn)
		} else if(p.isInstanceOf[Years]){
			new Duration2(p.asInstanceOf[Years].multipliedBy(n),groundFn)
		} else {
			throw new IllegalStateException("Cannot multiply Duration2")
		}
	}
	def seconds:Long = {
		var period = p.toPeriod
		val monthContrib:Long = period.getMonths*30*24*60*60
		val yearContrib:Long = period.getYears.longValue*365*24*60*60
		period = period.withMonths(0).withYears(0)
		period.toStandardDuration.getStandardSeconds+monthContrib+yearContrib
	}
	def <(other:Duration2) = this.seconds < other.seconds
	def >(other:Duration2) = this.seconds > other.seconds
	def ~(o:Any):Boolean = {
		val other:Duration2 = if(o.isInstanceOf[ReadablePeriod]){
				new Duration2(o.asInstanceOf[ReadablePeriod])
			} else if(o.isInstanceOf[Duration2]){
				o.asInstanceOf[Duration2]
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
		val other:Duration2 = if(o.isInstanceOf[ReadablePeriod]){
				new Duration2(o.asInstanceOf[ReadablePeriod])
			} else if(o.isInstanceOf[Duration2]){
				o.asInstanceOf[Duration2]
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


// --- Time2 ---
case class Time2(base:DateTime, offset:Duration2, modifiers:List[Time2=>Time2]) {
	def this(base:DateTime,offset:Duration2) = this(base, offset, null)
	def this(base:DateTime) = this(base, null, null)
	def this(offset:Duration2) = this(null, offset, null)

	def isGrounded:Boolean = this.base != null
	def alsoMod(mods:List[Time2=>Time2]):Time2 = {
		assert(mods != null, "adding mod to Time2, but mod is null")
		assert(!this.isGrounded, "adding modifier to grounded Time2")
		val existingMods = if(modifiers == null) List() else modifiers
		if(offset == null){
			assert(existingMods != null, "Appending to null mods")
			new Time2(base, null, mods ::: existingMods)
		} else {
			new Time2(base, null, mods ::: (((t:Time2)=>t+offset) :: existingMods) )
		}
	}
	def alsoMod(other:Time2):Time2 = {
		if(other.modifiers != null){ alsoMod(other.modifiers) } else { this }
	}

	def ground:Instant = {
		if(!isGrounded){
			throw new TimeException("Trying to ground ungrounded Time2")
		}else{
			var rtn:DateTime = base
			//(apply modifiers)
			if(modifiers != null){
				modifiers.reverse.foreach( (mod:Time2=>Time2) => {
					val cand = mod(new Time2(rtn))
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

	def ::(ground:Time2=>Time2):Time2 = {
		if(ground == null){ return this }
		if(isGrounded){
			ground(this)
		} else {
			val existingMods = if(modifiers == null) List() else modifiers
			if(offset == null){
				new Time2(base, offset, ground :: existingMods)
			} else {
				new Time2(base, null, ground :: (((t:Time2)=>t+offset) :: existingMods) )
			}
		}
	}
	def +(diff:Duration2):Time2 = {
		val added = if(isGrounded){
			//(case: adding to grounded Time2)
			assert(offset == null, "Offset should be null")
			val diffMillis = diff.seconds*1000
			val baseMillis = base.getMillis
			if(diffMillis > 0 && baseMillis > Long.MaxValue-diffMillis){
				//((overflow))
				new Time2(new DateTime(Long.MaxValue),offset,modifiers)
			} else if(diffMillis < 0 && baseMillis < Long.MinValue-diffMillis ){
				//((underflow))
				new Time2(new DateTime(Long.MinValue),offset,modifiers)
			} else {
				//((normal))
				try{
					new Time2(base.plus(diff),offset,modifiers)
				} catch {
					case (e:ArithmeticException) => 
						new Time2(base.plus(diffMillis),offset,modifiers) //catch-all
				}
			}
		}else if(offset != null){
			//(case: adding to existing offset)
			new Time2(base, offset+diff, modifiers)
		}else{
			//(case: adding new offset)
			new Time2(base, diff, modifiers)
		}
		assert(!added.isGrounded || added.modifiers == null,
			"modifiers on grounded Time2")
		diff.grounding :: added
	}
	def -(other:Time2):Duration2 = {
		//(error checks)
		if( (!this.isGrounded && other.isGrounded) ||
				(this.isGrounded && !other.isGrounded) ){
			throw new IllegalStateException(
				"Cannot subtract grounded and ungrounded Time2s");
		}
		if(this.isGrounded){
			//(case: subtracting grounded Time2s)
			assert(this.equals(Time2.DAWN_OF) || this != Time2.DAWN_OF, "eq check")
			assert(this.equals(Time2.END_OF) || this != Time2.END_OF, "eq check")
			val tM:Long = this.ground.getMillis
			val oM:Long = other.ground.getMillis
			if(this == Time2.DAWN_OF){
				//(case: subtracting from neg_infinity)
				if(other == Time2.DAWN_OF){ Duration2.ZERO }
				else { Duration2.NEG_INFINITE }
			} else if(this == Time2.END_OF){
				//(case: subtracting from pos_infinity)
				if(other == Time2.END_OF){ Duration2.ZERO }
				else { Duration2.INFINITE }
			} else if(oM < 0 && Long.MaxValue + oM < tM){
				//(case: overflowing a Long)
				Duration2.INFINITE
			} else if(oM > 0 && Long.MinValue + oM > tM){
				//(case: underflowing a Long)
				Duration2.NEG_INFINITE
			} else {
				//(case: normal subtraction)
				try {
					new Period(tM-oM)
				} catch {
					//(case: overflowed precise fields)
					case (e:ArithmeticException) => 
						new Period(new Period(tM-oM,PeriodType.years),PeriodType.standard)
				}
			}
		} else {
			//(case: approximating subtraction)
			val thisSec = if(this.offset == null) 0 else this.offset.seconds
			val thatSec = if(other.offset == null) 0 else other.offset.seconds
			Seconds.seconds((thisSec - thatSec).asInstanceOf[Int])
		}
	}
	def -(diff:Duration2):Time2 = {
		val subtracted = if(isGrounded){
			//(case: subtracting grounded Time2s)
			assert(offset == null, "Offset should be null for grounded Time2")
			val diffMillis = diff.seconds*1000
			val baseMillis = base.getMillis
			if( diffMillis > 0 && baseMillis < Long.MinValue+diffMillis ){
				//((underflow))
				new Time2(new DateTime(Long.MinValue),offset,modifiers)
			} else if(diffMillis < 0 && baseMillis > Long.MaxValue+diffMillis){
				//((overflow))
				new Time2(new DateTime(Long.MaxValue),offset,modifiers)
			} else {
				//((normal))
				new Time2(base.minus(diff),offset,modifiers)
			}
		}else if(offset != null){
			//(case: existing offset)
			new Time2(base, offset-diff, modifiers)
		}else{
			//(case: no offset)
			new Time2(base, ZERO.minus(diff), modifiers)
		}
		assert(!subtracted.isGrounded || subtracted.modifiers == null,
			"modifiers on grounded Time2")
		diff.grounding :: subtracted
	}
	def apply(grnd:Time2):Time2 = {
		var ground = grnd
		//--Argument check
		if(isGrounded){ throw new TimeException("Time2 is already grounded!") }
		if(!ground.isGrounded){ throw new TimeException("Argument not grounded!") }
		assert(ground.offset == null, "Ground should not have an offset")
		assert(ground.modifiers == null, "Ground should not have modifiers")
		//--Modifiers
		if(modifiers != null){
			modifiers.reverse.foreach( (mod:Time2=>Time2) => {
				ground = mod(ground)
			})
		}
		//--Offset
		if(offset != null){
			ground = ground + offset
		}
		//--Return
		new Time2(ground.base, null, null)
	}
	def accepts(s:Symbol):Boolean = {
		!isGrounded && s == 'Time2
	}
	def typeTag:Symbol = 'Time2

	def ~(o:Any):Boolean = {
		if(o.isInstanceOf[Time2] || o.isInstanceOf[DateTime]){
			val other:Time2 = o.asInstanceOf[Time2]
			if(this.isGrounded && other.isGrounded) {
				//(case: both grounded)
				return this.ground.getMillis == other.ground.getMillis
			} else if(!this.isGrounded && !other.isGrounded) {
				//(case: both ungrounded)
				val thisOffset:Duration2 
					= if(this.offset==null) Period.ZERO else this.offset
				val otherOffset:Duration2 
					= if(other.offset==null) Period.ZERO else other.offset
				return (thisOffset-otherOffset).seconds == 0 &&
					Time2.probablyEqual(this.modifiers,other.modifiers)
			}
		}
		return false
	}
	override def equals(o:Any):Boolean = {
		if(o.isInstanceOf[Time2] || o.isInstanceOf[DateTime]){
			val other:Time2 = o.asInstanceOf[Time2]
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
// Time2 OBJECTS
//------------------------------------------------------------------------------

object Lex {
	object LexUtil {
		def dow:(Time2,Int)=>Range2 = (t:Time2,iArg:Int) => {
			val i:Int = if(iArg < 0) t.base.getDayOfWeek else iArg
			val begin:Time2 = if(t.isGrounded){
					Time2(t.base.withDayOfWeek(i).withMillisOfDay(0), null)
				} else {
					new Time2(null,null)
				}
			Range2(begin, begin+Days.ONE)
		}
		def dom:(Time2,Int)=>Range2 = (t:Time2,iArg:Int) => {
			val i:Int = if(iArg < 0) t.base.getDayOfMonth else iArg
			if(i <= 0){throw new IllegalArgumentException("negative day of month")}
			val begin:Time2 = if(t.isGrounded){
					try{
						Time2(t.base.withDayOfMonth(i).withMillisOfDay(0), null)
					} catch { case (e:IllegalFieldValueException) => 
						Time2(t.base.withDayOfMonth(1).withMillisOfDay(0),null)+MONTH-DAY
					}
				} else {
					new Time2(null,null)
				}
			Range2(begin, begin+Days.ONE)
		}
		def woy:(Time2,Int)=>Range2 = (t:Time2,iArg:Int) => {
			val i:Int = if(iArg < 0) t.base.getWeekOfWeekyear else iArg
			if(i <= 0){throw new IllegalArgumentException("negative week of year")}
			val begin:Time2 = if(t.isGrounded){
					try{
						Time2(t.base.withWeekOfWeekyear(i)
							.withDayOfWeek(1).withMillisOfDay(0), null)
					} catch { case (e:IllegalFieldValueException) => 
						Time2(t.base.withWeekOfWeekyear(i)
							.withDayOfWeek(1).withMillisOfDay(0), null)+YEAR-WEEK
					}
				} else {
					new Time2(null,null)
				}
			Range2(begin, begin+Weeks.ONE)
		}
		def moy:(Time2,Int)=>Range2 = (t:Time2,iArg:Int) => {
			val i:Int = if(iArg < 0) t.base.getMonthOfYear else iArg
			val begin:Time2 = if(t.isGrounded){
					Time2(t.base.withMonthOfYear(i)
						.withDayOfMonth(1).withMillisOfDay(0), null)
				} else {
					new Time2(null,null)
				}
			Range2(begin, begin+Months.ONE)
		}
		def qoy:(Time2,Int)=>Range2 = (t:Time2,iArg:Int) => {
			val i:Int = if(iArg < 0) ((t.base.getMonthOfYear-1)%3)+1 else iArg
			val begin:Time2 = if(t.isGrounded){
					Time2(t.base.withMonthOfYear(3*(i-1)+1)
						.withDayOfMonth(1).withMillisOfDay(0), null)
				} else {
					new Time2(null,null)
				}
			Range2(begin, begin+Months.THREE)
		}
		def yoc:(Time2,Int)=>Range2 = (t:Time2,iArg:Int) => {
			val i:Int = if(iArg < 0) 0 else iArg
			val begin:Time2 = if(t.isGrounded){
					val newYear = t.base.getYear - (t.base.getYear%100) + i
					try{
						Time2(t.base.withYear(newYear)
							.withMonthOfYear(1).withDayOfMonth(1).withMillisOfDay(0), null)
					} catch { case (e:IllegalFieldValueException) => 
						if(newYear < 0){
							Time2.DAWN_OF
						}else{
							Time2.END_OF
						}
					}
				} else {
					new Time2(null,null)
				}
			Range2(begin, begin+Years.ONE)
		}
	}
	//--Duration2s
	val SEC:Duration2 = Seconds.ONE
	val MIN:Duration2 = Minutes.ONE
	val HOUR:Duration2 = Hours.ONE
	val DAY:Duration2 = Days.ONE
	val WEEK:Duration2 = Weeks.ONE
	val MONTH:Duration2 = Months.ONE
	val QUARTER:Duration2 = Months.THREE
	val YEAR:Duration2 = Years.ONE
	//--Misc
	val ZERO:Period = Seconds.ZERO.toPeriod
	val NOW:Time2 = new Time2(null,null)
	val TODAY:Range2 = {
		val begin:Time2 = Time2(null,null,List[Time2=>Time2]( (t:Time2) => {
			assert(t.isGrounded, "grounding today with ungrounded Time2")
			assert(t.equals(Time2.DAWN_OF) || t != Time2.DAWN_OF, "eq check")
			assert(t.equals(Time2.END_OF) || t != Time2.END_OF, "eq check")
			if(t == Time2.DAWN_OF){
				//(case: negative infinity)
				t
			} else if(t == Time2.END_OF){
				//(case: positive infinity)
				t-DAY
			} else {
				//(case: floor to date)
				Time2(t.base.withMillisOfDay(0),t.offset,t.modifiers)
			}
		}) )
		Range2(begin,begin+DAY)
	}
	val ALL_Time2:Range2 = Range2(Time2.DAWN_OF,Time2.END_OF)
	val NODUR:Duration2 = DAY
	//--Day of Week
	val MON:Duration2 = new Duration2(Weeks.ONE, LexUtil.dow(_,1))
	val TUE:Duration2 = new Duration2(Weeks.ONE, LexUtil.dow(_,2))
	val WED:Duration2 = new Duration2(Weeks.ONE, LexUtil.dow(_,3))
	val THU:Duration2 = new Duration2(Weeks.ONE, LexUtil.dow(_,4))
	val FRI:Duration2 = new Duration2(Weeks.ONE, LexUtil.dow(_,5))
	val SAT:Duration2 = new Duration2(Weeks.ONE, LexUtil.dow(_,6))
	val SUN:Duration2 = new Duration2(Weeks.ONE, LexUtil.dow(_,7))
	//--OTHER Duration2S
	def DOW(i:Int) = new Duration2(Weeks.ONE, LexUtil.dow(_,i))
	def DOM(i:Int) = new Duration2(Months.ONE, LexUtil.dom(_,i))
	def WOY(i:Int) = new Duration2(Years.ONE, LexUtil.woy(_,i))
	def MOY(i:Int) = new Duration2(Years.ONE, LexUtil.moy(_,i))
	def QOY(i:Int) = new Duration2(Years.ONE, LexUtil.qoy(_,i))
	def YOC(i:Int) = new Duration2(Years.years(100), LexUtil.yoc(_,i))
	def YEAR(i:Int) = new Range2(Time2(i),Time2(i+1))
	def DECADE(i:Int) = new Range2(Time2(i*10),Time2((i+1)*10))
	def CENTURY(i:Int) = new Range2(Time2(i*100),Time2((i+1)*100))
	val AYEAR = new Duration2(Years.ONE, (t:Time2) => {
			val begin:Time2 = if(t.isGrounded){
					Time2(t.base.withDayOfYear(1).withMillisOfDay(0), null)
				} else {
					new Time2(null,null)
				}
			Range2(begin, begin+Years.ONE)
		})
	
	//--Shifts
	val shiftLeft:(Range2,Duration2)=>Range2 = _ << _
	val shiftRight:(Range2,Duration2)=>Range2 = _ >> _
	val catLeft:(Range2,Duration2)=>Range2 = _ <| _
	val catRight:(Range2,Duration2)=>Range2 = _ |> _
	val shrinkBegin:(Range2,Duration2)=>Range2 = _ |< _
	val shrinkEnd:(Range2,Duration2)=>Range2 = _ >| _
	val intersect:(Range2,Range2)=>Range2 = _ ^ _
	val cons:(Range2,Range2)=>Range2 = _.cons(_)

	def todaysDate:Time2 = Time2((new DateTime).withMillisOfDay(0))
}

object Range2 {
}

object Duration2 {
	def apply(millis:Long):Duration2 = new Period(millis)
	val INFINITE:Duration2 = Period.years(Int.MaxValue)
	val NEG_INFINITE:Duration2 = Period.years(Int.MinValue)
	val ZERO:Duration2 = new Period(0L)
}

object Time2 {
	val DAWN_OF = new Time2(new DateTime(Long.MinValue), null, null)
	val END_OF = new Time2(new DateTime(Long.MaxValue), null, null)

	def apply(base:DateTime) = new Time2(base, null, null)
	def apply(base:DateTime, offset:Duration2) = new Time2(base, offset, null)

	def apply(year:Int, month:Int, day:Int, hour:Int, min:Int, sec:Int):Time2 = {
		new Time2(new DateTime(year,month,day,hour,min,sec,0))
	}
	def apply(year:Int, month:Int, day:Int, hour:Int, min:Int):Time2 = {
		apply(year, month, day, hour, min, 0)
	}
	def apply(year:Int, month:Int, day:Int, hour:Int):Time2 = {
		apply(year, month, day, hour, 0, 0)
	}
	def apply(year:Int, month:Int, day:Int):Time2 =apply(year, month, day, 0, 0, 0)
	def apply(year:Int, month:Int):Time2 = apply(year, month, 1, 0, 0, 0)
	def apply(year:Int):Time2 = apply(year, 1, 1, 0, 0,0)

	def probablyEqual(a:Time2=>Time2, b:Time2=>Time2):Boolean = {
		val preDaylightSavings = Time2(2011,3,12,4,26)
		val postDaylightSavings = Time2(2011,3,13,9,12)
		val random = Time2(1985,10,4)
		(a(preDaylightSavings) ~ b(preDaylightSavings)) &&
			(a(postDaylightSavings) ~ b(postDaylightSavings)) &&
			(a(random) ~ b(random))
	}
	def probablyEqual(a:List[Time2=>Time2], b:List[Time2=>Time2]):Boolean = {
		if(a == null && b == null){ return true }
		if(a != null && b != null){
		if(a.length != b.length){ return false }
			a.zip(b).forall{ case (ai,bi) => probablyEqual(ai,bi) }
		} else {
			false
		}
	}
	def probablyEqualRange2(a:Range2=>Range2, b:Range2=>Range2):Boolean = {
		import scala.util.Random
		val r:Random = new Random(42)
		for( i <- 1 until 100 ) {
			val arg:Range2 = Range2(
				Time2(r.nextInt(3000),r.nextInt(11)+1,r.nextInt(27)+1,
					r.nextInt(24),r.nextInt(60),r.nextInt(60)),
				Time2(r.nextInt(3000),r.nextInt(11)+1,r.nextInt(27)+1,
					r.nextInt(24),r.nextInt(60),r.nextInt(60)))
			if( !(a(arg) ~ b(arg)) ){
				return false
			}
		}
		return true
	}

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
			interpreter.interpret("import time.Conversions._")
			interpreter.interpret("val ground = Time2(2011,4,26)")
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


	def main(args:Array[String]):Unit = {
		interactive
	}
}
