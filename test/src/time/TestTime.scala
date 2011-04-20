package time

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import time._
import time.Lex._
import org.joda.time._

class TimeSpec extends Spec with ShouldMatchers{
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
}


class RangeSpec extends Spec with ShouldMatchers{
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
		it("should be extendable (right)"){
			assert((Range(Time(2011),Time(2012)) |> YEAR) 
				== Range(Time(2011),Time(2013)))
			assert((Range(Time(2011,1,1),Time(2012,2,1)) |> DAY) == 
				Range(Time(2011,1,1),Time(2012,2,2)))
		}
		it("should be extendable (left)"){
			assert((Range(Time(2011),Time(2012)) <| YEAR) 
				== Range(Time(2010),Time(2012)))
			assert((Range(Time(2011,1,1),Time(2012,2,1)) <| DAY) == 
				Range(Time(2010,12,31),Time(2012,2,1)))
		}
		it("should be shrinkable (right)"){
			assert((Range(Time(2011),Time(2012)) |< YEAR) 
				== Range(Time(2011),Time(2011)))
			assert((Range(Time(2011,1,1),Time(2012,2,1)) |< DAY) == 
				Range(Time(2011,1,1),Time(2012,1,31)))
		}
		it("should be shrinkable (left)"){
			assert((Range(Time(2011),Time(2012)) >| YEAR) 
				== Range(Time(2012),Time(2012)))
			assert((Range(Time(2011,1,1),Time(2012,2,1)) >| DAY) == 
				Range(Time(2011,1,2),Time(2012,2,1)))
		}
	}
}



