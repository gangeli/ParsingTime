package time

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import time._
import time.Lex._
import org.joda.time._

class BehaviorSpec extends Spec with ShouldMatchers{
	describe("A Time") {
		it("should be creatable"){ 
			new Time(null,null) 
			Time(2012)
			Time(2012,12)
			Time(2012,12,31)
			Time(2012,12,31,0)
			Time(2012,12,31,0,0)
			Time(2012,12,31,0,0,0)
		}
		it("should exception on bad values"){
			evaluating { Time(2012,-1) } should produce [IllegalFieldValueException]
			evaluating { Time(2012,13) } should produce [IllegalFieldValueException]
			evaluating { Time(2012,12,-1) 
				} should produce [IllegalFieldValueException]
			evaluating { Time(2012,12,32) 
				} should produce [IllegalFieldValueException]
			evaluating { Time(2012,12,31,-1) 
				} should produce [IllegalFieldValueException]
			evaluating { Time(2012,12,31,25) 
			} should produce [IllegalFieldValueException]
			evaluating { Time(2012,12,31,0,-1) 
				} should produce [IllegalFieldValueException]
			evaluating { Time(2012,12,31,0,60) 
				} should produce [IllegalFieldValueException]
			evaluating { Time(2012,12,31,0,0,-1) 
				} should produce [IllegalFieldValueException]
			evaluating { Time(2012,12,31,0,0,60) 
				} should produce [IllegalFieldValueException]
		}
		//...
		it("should have equality hold"){
			assert(Time(2012) == Time(2012))
			assert(Time(2010) != Time(2012))
			assert(Time(2012,12,31,0,0,0) == Time(2012,12,31,0,0,0))
			assert(Time(2012,12,31,0,0,0) != Time(2012,12,31,0,0,1))
		}
		it("should have equality hold when ranges are applied"){
			assert(Time(2010,12)+DAY == Time(2010,12)+DAY)
			assert(Time(2010,12)+DAY != Time(2010,12)+(DAY*2))
			assert(Time(2010,12,1)+DAY == Time(2010,12,2))
			assert(Time(2010,12,1)+HOUR*24 == Time(2010,12,2))
			assert(Time(2010,12,1)+HOUR*24 == Time(2010,12,1)+DAY)
		}
		it("should have simple equality hold when times are abstract"){
			assert(NOW == NOW)
			assert(NOW+DAY == NOW+DAY)
			assert(NOW+WEEK == NOW+WEEK)
		}
		it("should not in general have complex equality"){
			assert(NOW+HOUR*24 != NOW+DAY)
			assert( (NOW+HOUR*24)(Time(2010)) == (NOW+DAY)(Time(2010)) )
		}
		it("should have likely equality hold"){
			assert((Time(2010,12)+DAY) ~ (Time(2010,12)+DAY))
			assert(!((Time(2010,12)+DAY) ~ (Time(2010,12)+(DAY*2))))
			assert((Time(2010,12,1)+DAY) ~ (Time(2010,12,2)))
			assert((Time(2010,12,1)+HOUR*24) ~ (Time(2010,12,2)))
			assert((Time(2010,12,1)+HOUR*24) ~ (Time(2010,12,1)+DAY))
			assert((NOW) ~ (NOW-WEEK+WEEK))
			assert((NOW+HOUR*24) ~ (NOW+DAY))
		}
	}
	
	describe("A Range") {
		it("should be creatable"){ 
			Range(Time(2011),Time(2012))
		}
		it("should have equality hold"){
			assert(Range(Time(2011),Time(2012)) == Range(Time(2011), Time(2012)))
			assert(Range(Time(2011),Time(2012)) != Range(Time(2011), Time(2000)))
			assert(Range(Time(2011)+DAY,Time(2012)) == 
				Range(Time(2011)+DAY, Time(2012)))
		}
		it("should have likely equality hold"){
			assert(Range(Time(2011)+WEEK,Time(2012)+MONTH*3) ~ 
				Range(Time(2011)+DAY*7, Time(2012)+MONTH*3))
		}
		it("should be shiftable"){
			assert((Range(Time(2011),Time(2012)) >> YEAR) == 
				Range(Time(2012),Time(2013)))
			assert((Range(Time(2011,1,1),Time(2012,2,1)) >> DAY) != 
				Range(Time(2012,1,2),Time(2012,2,2)))
		}
		it("should be catable (onto right)"){
			assert((Range(Time(2011),Time(2012)) |> YEAR) 
				== Range(Time(2012),Time(2013)))
			assert((Range(Time(2011,1,1),Time(2012,2,1)) |> DAY) == 
				Range(Time(2012,2,1),Time(2012,2,2)))
		}
		it("should be catable (onto left)"){
			assert((Range(Time(2011),Time(2012)) <| YEAR) 
				== Range(Time(2010),Time(2011)))
			assert((Range(Time(2011,1,1),Time(2012,2,1)) <| DAY) == 
				Range(Time(2010,12,31),Time(2011,1,1)))
		}
		it("should be shrinkable (starting left)"){
			assert((Range(Time(2011),Time(2012)) |< YEAR) 
				== Range(Time(2011),Time(2012)))
			assert((Range(Time(2011,1,1),Time(2012,2,1)) |< DAY) == 
				Range(Time(2011,1,1),Time(2011,1,2)))
		}
		it("should be shrinkable (ending right)"){
			assert((Range(Time(2011),Time(2012)) >| YEAR) 
				== Range(Time(2011),Time(2012)))
			assert((Range(Time(2011,1,1),Time(2012,2,1)) >| DAY) == 
				Range(Time(2012,1,31),Time(2012,2,1)))
		}
	}
	
	describe("A Grounded Duration") {
		it("should be a duration"){
			assert(FRI ~ WEEK)
			assert(DOM(12) ~ MONTH)
		}
		it("should be groundable in a range"){
			assert(FRI.ground(Range(Time(2011,4,25),Time(2011,5,2)))
				== Range(Time(2011,4,29), Time(2011,4,30)) )
		}
		it("should be groundable in a grounded time"){
			assert(FRI.ground(Time(2011,4,25))
				== Range(Time(2011,4,29), Time(2011,4,30)) )
		}
		it("shouldn't be groundable in an abstract time"){
			evaluating{FRI.ground(NOW)} should produce [TimeException]
			evaluating{FRI.ground(Range(NOW,NOW+DAY))} should produce [TimeException]
		}
		it("should respect arithematic"){
			assert((FRI+DAY).ground(Time(2011,4,25))
				== Range(Time(2011,4,30), Time(2011,5,1)) )
		}
	}
}

class ExamplesSpec extends Spec with ShouldMatchers{
	describe("Primitives") {
		it("works for March 6 2011"){ Range(Time(2011,03,6),Time(2011,03,7)) }
		it("works for March 2011"){ Range(Time(2011,03,1),Time(2011,04,1)) }
		it("works for 2011"){ Range(Time(2011,01,1),Time(2012,01,1)) }
		it("works for Friday"){ FRI }
		it("works for March 5th")(pending)
		it("works for March")(pending)
		it("works for Today"){ NOW }
		it("works for Week"){ WEEK }
		it("works for Hour"){ HOUR }
		it("works for Year"){ YEAR }
	}

	describe("Cat Left") {
		it("works for Day before March 7 2011"){
			val m7th2011 = Range(Time(2011,03,07),Time(2011,03,8))
			val day = DAY
			val before = catLeft
			val target = Range(Time(2011,03,06),Time(2011,03,7))
		}
		it("works for Day before today"){
			val today = Range(NOW,NOW+DAY)
			val day = DAY
			val before = catLeft
			val target = Range(NOW-DAY,NOW)
			assert( before(today,day) ~ target )
		}
		it("works for Last week"){
			val today = Range(NOW,NOW+DAY)
			val week = WEEK
			val last = catLeft
			val target = Range(NOW-WEEK,NOW)
			assert( last(today,week) ~ target )
		}
		it("works for yesterday"){
			val yesterday = catLeft
			val target = Range(NOW-DAY,NOW)
			assert( yesterday(NOTIME,NODUR) ~ target )
		}
	}
	describe("Cat Right") {
		it("works for Day after March 7th 2011"){
			val day = DAY
			val after = catRight
			val m7th2011 = Range(Time(2011,03,7),Time(2011,03,8))
			val target = Range(Time(2011,03,8),Time(2011,03,9))
			assert( after(m7th2011,day) ~ target)
		}
		it("works for Day after today"){
			val day = DAY
			val after = catRight
			val today = Range(NOW,NOW+DAY)
			val target = Range(NOW+DAY,NOW+DAY*2)
			assert( after(today,day) ~ target)
		}
	}
	
	describe("Shrink to Begin") {
		it("works for First day of March")(pending)
	}

	describe("Complex Examples") {
		it("implement me")(pending)
	}
}





