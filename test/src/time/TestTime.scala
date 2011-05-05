package time

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import time._
import time.Lex._
import org.joda.time._

class RepresentationSpec extends Spec with ShouldMatchers{
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
		it("should be subtractable"){
			((NOW+DAY) - NOW) ~ DAY
			((NOW+DAY*2) - (NOW+DAY)) ~ DAY
		}
		it("should be chainable"){
			val march = (t:Time) => 
				new Time(t.base.withMonthOfYear(3).withDayOfMonth(1).withMillisOfDay(0))
			val fri = (t:Time) => 
				new Time(t.base.withDayOfWeek(5).withMillisOfDay(0))
			val time = new Time(null, DAY, List(fri,march))
			assert( time(Time(2011,04,30)) ~ Time(2011,03,5) )
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
		it("should be intersectable (grounded)"){
			assert( (Range(Time(2011),Time(2012)) ^ Range(Time(2011,10),Time(2012,2)))
				== Range(Time(2011,10),Time(2012)) )
		}
		it("should be intersectable (ungrounded)"){
			val ground = Time(2011,4,25)
			assert( (DOM(5)(NOW) ^ MOY(3)(NOW))(ground) 
				~ Range(Time(2011,3,5),Time(2011,3,6)) )
			assert( (MOY(3)(NOW) ^ DOM(5)(NOW))(ground) 
				~ Range(Time(2011,3,5),Time(2011,3,6)) )
		}
	}

	describe("A duration"){
		it("should have seconds"){
			assert(SEC.seconds === 1.0)
			assert(MIN.seconds === 60.0)
			assert(HOUR.seconds === 60.0*60.0)
			assert(DAY.seconds === 60.0*60.0*24)
			assert(WEEK.seconds === 60.0*60.0*24*7)
			assert(MONTH.seconds === 60.0*60.0*24*30)
			assert((MONTH*3).seconds === 60.0*60.0*24*30*3)
			assert(YEAR.seconds === 60.0*60.0*24*365)
			assert((YEAR*1000).seconds === 60.0*60.0*24*365*1000)
			assert(Duration.INFINITE > YEAR*1000)
		}
	}
	
	describe("A Grounded Duration") {
		it("should be a duration"){
			assert(FRI ~ WEEK)
			assert(DOM(12) ~ MONTH)
		}
		it("should be groundable in a grounded time"){
			assert(FRI(Time(2011,4,25))
				== Range(Time(2011,4,29), Time(2011,4,30)) )
		}
		it("should be groundable in an abstract time"){
			//(should be groundable)
			val thisFriday:Range = FRI(NOW)
			//(not equal to day)
			val aDay:Range = Range(NOW, NOW+DAY)
			assert(thisFriday != aDay)
			//(not same as day)
			assert(!(thisFriday ~ aDay))
			//(same as another friday)
			val thisFridayAgain:Range = FRI(NOW)
			assert(thisFriday ~ thisFridayAgain)
		}
		it("should respect arithemetic"){
			assert((FRI(NOW)>>DAY)(Time(2011,4,25))
				== Range(Time(2011,4,30), Time(2011,5,1)) )
		}
		it("should be intersectable"){
			val march = MOY(3)
			val fifth = DOM(5)

		}
	}
}

class ExamplesSpec extends Spec with ShouldMatchers{
	describe("Primitives") {
		it("works for March 6 2011"){ Range(Time(2011,03,6),Time(2011,03,7)) }
		it("works for March 2011"){ Range(Time(2011,03,1),Time(2011,04,1)) }
		it("works for 2011"){ Range(Time(2011,01,1),Time(2012,01,1)) }
		it("works for Friday"){ FRI }
		it("works for March 5th"){ MOY(3)(NOW) ^ DOM(5)(NOW) }
		it("works for March"){ MOY(3) }
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
		it("works for First day of March"){
			val first = shrinkBegin
			val day = DAY
			val march = MOY(3)
			val implicitNow = NOW
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,3,1),Time(2011,3,2))
			assert( first(march(implicitNow), day)(ground) ~ target )
		}
		it("works for First month of the year"){
			val first = shrinkBegin
			val month = MONTH
			val year = AYEAR
			val implicitNow = NOW
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,1,1),Time(2011,2,1))
			assert( first( year(implicitNow), month)(ground) ~ target )
		}
	}
	describe("Shrink to End"){
		it("works for Last day of March"){
			val last = shrinkEnd
			val day = DAY
			val march = MOY(3)
			val implicitNow = NOW
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,3,31),Time(2011,4,1))
			assert( last(march(implicitNow), day)(ground) ~ target )
		}
		it("works for Last week of March"){
			val last = shrinkEnd
			val week = WEEK
			val march = MOY(3)
			val implicitNow = NOW
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,3,25),Time(2011,4,1))
			assert( last(march(implicitNow), week)(ground) ~ target )
		}
	}
	describe("Intersect"){ 
		it("works for April of 2007"){
			val april = MOY(4)
			val y2007 = Range(Time(2007),Time(2008))
			val implicitNow = NOW
			val implicitIntersect = intersect
			val target = Range(Time(2007,4),Time(2007,5))
			assert( implicitIntersect(april(implicitNow), y2007) ~ target )
		}
		it("works for April last year"){
			val april = MOY(4)
			val year = YEAR
			val last = catLeft
			val implicitToday = Range(NOW,NOW+DAY)
			val implicitNow = NOW
			val implicitIntersect = intersect
			val ground = Time(2011,4,25)
			val target = Range(Time(2010,4),Time(2010,5))
			assert( 
				implicitIntersect(
					april(implicitNow), 
					last(implicitToday, year) )(ground)
				~ target )
		}
	}
	describe("Cons"){ 
		it("works for May 3 2010 to May 10 2010"){
			val may32010 = Range(Time(2010,5,3),Time(2010,5,4))
			val may102010 = Range(Time(2010,5,10),Time(2010,5,11))
			val to = cons
			val target = Range(Time(2010,5,3),Time(2010,5,11))
			assert( to(may32010,may102010) ~ target)
		}
		it("works for Since May 3 2010"){
			val may32010 = Range(Time(2010,5,3),Time(2010,5,4))
			val since = cons
			val target = (r:Range) => Range(Time(2010,5,3), r.end)
			assert( Time.probablyEqualRange(target,since(may32010,_:Range)) )
		}
		it("works for Since yesterday"){
			val yesterday = catLeft(NOTIME,NODUR)
			val since = cons
			val target = (r:Range) => Range(NOW-DAY, r.end)
			assert( Time.probablyEqualRange(target,since(yesterday,_:Range)) )
		}
		it("works for Until tomorrow"){
			val tomorrow = catRight(NOTIME,NODUR)
			val until = cons
			val target = (r:Range) => Range(r.begin,NOW+DAY*2)
			assert( Time.probablyEqualRange(target,until(_:Range,tomorrow)) )
		}
		it("works for Before tomorrow"){
			val tomorrow = catRight(NOTIME,NODUR)
			val before = cons
			val target = (r:Range) => Range(r.begin,NOW+DAY*2)
			assert( Time.probablyEqualRange(target,before(_:Range,tomorrow)) )
		}
		it("works for yesterday until tomorrow"){
			val yesterday = catLeft(NOTIME,NODUR)
			val tomorrow = catRight(NOTIME,NODUR)
			val until = cons
			val target = Range(NOW-DAY,NOW+DAY*2)
			assert( until(yesterday,tomorrow) ~ target )
		}
	}
	describe("Numbers"){ 
		it("works for 24 hours"){
			val hours = HOUR
			assert( (hours*24) ~ DAY )
		}
		it("works for third day"){
			val third = 3
			val day = DAY
			assert( (day*third) ~ (DAY*3) )
		}
	}

	describe("Complex Examples") {
		it("works for Third quarter"){
			val third = 3
			val quarter = QOY(third)
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,7,1),Time(2011,10,1))
			assert( quarter(ground) ~ target )
		}
		it("works for This year"){
			val year = AYEAR
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,1,1),Time(2012,1,1))
			assert( year(ground) ~ target )
		}
		it("works for A year ago"){
			val year = YEAR
			val ago = shiftLeft
			val implicitToday = Range(NOW,NOW+DAY)
			val ground = Time(2011,4,25)
			val target = Range(Time(2010,4,25),Time(2010,4,26))
			assert( ago(implicitToday,year)(ground) ~ target )
		}
		it("works for The year ending June 30 1990"){
			val year = YEAR
			val ending = shrinkEnd
			val june = MOY(6)(NOW)
			val d30 = DOM(30)(NOW)
			val y1990 = Range(Time(1990),Time(1991))
			val target = Range(Time(1989,7,1), Time(1990,7,1))
			assert( ending( (june ^ d30) ^ y1990, year ) ~ target )
		}
		it("works for The 9 months"){
			val nine = 9
			val months = MONTH
			assert( (months*nine) ~ (MONTH*9) )
		}
		it("works for The latest quarter"){
			val quarter = QOY(-1)
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,1,1),Time(2011,4,1))
			assert( quarter(ground) ~ target)
		}
	}
	
	describe("Other Examples") {
		it("works for Friday last week"){
			val friday = FRI
			val implicitNow = NOW
			val implicitToday = Range(NOW, NOW+DAY)
			val last = catLeft
			val week = WEEK
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,4,22),Time(2011,4,23))
			assert( (friday(implicitNow) ^ last(implicitToday,week))(ground)
				~ target)
		}
		it("works for Friday last month"){
			val friday = FRI
			val implicitNow = NOW
			val implicitToday = Range(NOW, NOW+DAY)
			val last = catLeft
			val month = MONTH
			val ground = Time(2011,4,8)
			val target = Range(Time(2011,3,11),Time(2011,3,12))
			assert( (friday(implicitNow) ^ last(implicitToday,month))(ground)
				~ target)
		}
		it("works for The day before last Friday"){
			val day = DAY
			val before = catLeft
			val last = catLeft
			val friday = FRI
			val implicitNow = MON(NOW)
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,4,21),Time(2011,4,22))
			assert( before(last(implicitNow,friday),day)(ground) ~ target )
		}
		it("works for This week"){
			val week = WEEK
			val ths = shrinkBegin
			val implicitNow = MON(NOW)
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,4,25),Time(2011,5,2))
			assert( ths(implicitNow,week)(ground) ~ target )
		}
		it("works for Friday this week"){
			val friday = FRI
			val week = WEEK
			val ths = shrinkBegin
			val implicitNow = MON(NOW)
			val implicitIntersect = intersect
			val ground = Time(2011,4,25)
			val target = Range(Time(2011,4,29),Time(2011,4,30))
			assert( implicitIntersect(ths(implicitNow,week),friday(NOW))(ground) 
				~ target )
		}
	}
}


class EvaluationMethodSpec extends Spec with ShouldMatchers{
	describe("The Scoring Procedure") {
		it("evaluates to 1 for exact matches"){
			expect(1.0){
				Score.score( (0.0,0.0) )
			}
		}
		it("evaluates to 0 for exact mismatches"){
			expect(0.0){
				Score.score( 
					(java.lang.Double.POSITIVE_INFINITY,
					 java.lang.Double.POSITIVE_INFINITY ) )
			}
		}
		it("evaluates to the right value on default c"){
			Score.score((-5.0, 10.0)) should be (0.1287878 plusOrMinus(0.0001))
			Score.score((5.0, 10.0)) should be (0.1287878 plusOrMinus(0.0001))
			Score.score((10.0, -5.0)) should be (0.1287878 plusOrMinus(0.0001))
			Score.score((-10.0, -5.0)) should be (0.1287878 plusOrMinus(0.0001))
		}
		it("evaluates to the right value for different c's"){
			Score.score((-5.0, 10.0), 2.0, 0.5) should 
				be (0.226190 plusOrMinus(0.0001))
			Score.score((5.0, 10.0), 2.0, 0.5) should 
				be (0.1287878787 plusOrMinus(0.0001))
			Score.score((-5.0, -10.0), 2.0, 0.5) should 
				be (0.166666666 plusOrMinus(0.0001))
			Score.score((5.0, -10.0), 2.0, 0.5) should 
				be (0.069264 plusOrMinus(0.0001))
		}
	}

	describe("Range Difference Procedure") {
		import ParseConversions._
		val ground = Time(2011,4,26)
		//--Parse is Range
		it("works for grounded range / grounded range"){
			val guess = Range(Time(2011,04,26),Time(2011,04,26))
			val parse:Parse = Range(Time(2011,04,26),Time(2011,04,26))
			U.sumDiff(parse.rangeDiff(guess,ground)) should be (0.0)
			val guessBad = Range(Time(2011,04,26),Time(2011,04,27))
			U.sumDiff(parse.rangeDiff(guessBad,ground)) should be (60.0*60.0*24.0)
		}
		it("works for ungrounded range / ungrounded range"){
			val parse:Parse = Range(NOW,NOW+DAY)
			val guess = Range(NOW,NOW+DAY)
			U.sumDiff(parse.rangeDiff(guess,ground)) should be (0.0)
			val guessBad = Range(NOW-DAY,NOW)
			U.sumDiff(parse.rangeDiff(guessBad,ground)) should be (60.0*60.0*24.0*2)
		}
		it("works for grounded range / ungrounded range"){
			val parse:Parse = Range(NOW,NOW+DAY)
			val guess = Range(Time(2011,04,26),Time(2011,04,27))
			U.sumDiff(parse.rangeDiff(guess,ground)) should be (0.0)
			val guessBad = Range(Time(2011,04,25),Time(2011,04,26))
			U.sumDiff(parse.rangeDiff(guessBad,ground)) should be (60.0*60.0*24.0*2)
		}
		it("works for ungrounded range / grounded range"){
			val parse:Parse = Range(Time(2011,04,26),Time(2011,04,27))
			val guess = Range(NOW,NOW+DAY)
			U.sumDiff(parse.rangeDiff(guess,ground)) should be (0.0)
			val guessBad = Range(NOW-DAY,NOW)
			U.sumDiff(parse.rangeDiff(guessBad,ground)) should be (60.0*60.0*24.0*2)
		}
		it("works for grounded range / duration"){
			val parse:Parse = Range(Time(2011,04,26),Time(2011,04,27))
			val guess = DAY
			U.sumDiff(parse.durationDiff(guess,ground)
				) should be (java.lang.Integer.MAX_VALUE)
		}
		it("works for ungrounded range / duration"){
			val parse:Parse = Range(NOW,NOW+DAY)
			val guess = DAY
			U.sumDiff(parse.durationDiff(guess,ground)
				) should be (java.lang.Integer.MAX_VALUE)
		}
		it("works for grounded range / function"){
			val parse:Parse = Range(Time(2011,04,26),Time(2011,04,27))
			var guess = (r:Range) => Range(Time(2011,04,26),Time(2011,04,27))
			U.sumDiff(parse.fnDiff(guess,ground)) should be (0.0)
			guess = (r:Range) => Range(NOW,Time(2011,04,27))
			U.sumDiff(parse.fnDiff(guess,ground)) should be (0.0)
			guess = (r:Range) => Range(r.begin,Time(2011,04,27))
			U.sumDiff(parse.fnDiff(guess,ground)) should be (Integer.MAX_VALUE)
			guess = (r:Range) => Range(Time(2011,04,26),r.end)
			U.sumDiff(parse.fnDiff(guess,ground)) should be (Integer.MAX_VALUE)
		}
		//--Parse is Duration
		it("works for duration / duration"){
			val parse:Parse = WEEK
			var guess = WEEK
			U.sumDiff(parse.durationDiff(guess,ground)) should be (0.0)
			guess = DAY
			U.sumDiff(parse.durationDiff(guess,ground)) should be (60.0*60*24*6)
		}
		it("works for grounded duration / duration"){
			val parse:Parse = FRI
			var guess = WEEK
			U.sumDiff(parse.durationDiff(guess,ground)) should be (0.0)
			guess = DAY
			U.sumDiff(parse.durationDiff(guess,ground)) should be (60.0*60*24*6)
		}
		it("works for duration / range"){
			val parse:Parse = WEEK
			var guess = Range(Time(2011,4,26),Time(2011,4,27))
			U.sumDiff(parse.rangeDiff(guess,ground)) should be (Integer.MAX_VALUE)
			guess = Range(NOW,Time(2011,4,27))
			U.sumDiff(parse.rangeDiff(guess,ground)) should be (Integer.MAX_VALUE)
			guess = Range(NOW,NOW+DAY)
			U.sumDiff(parse.rangeDiff(guess,ground)) should be (Integer.MAX_VALUE)
		}
		//--Parse is Function
		it("works for function / function"){
			//(normal)
			var parse:Parse = (r:Range) => Range(Time(2011,04,26),Time(2011,04,27))
			var guess = (r:Range) => Range(Time(2011,04,26),Time(2011,04,27))
			U.sumDiff(parse.fnDiff(guess,ground)) should be (0)
			guess = (r:Range) => Range(Time(2011,04,26),Time(2011,04,28))
			U.sumDiff(parse.fnDiff(guess,ground)) should be (60.0*60*24)
			//(needs grounding)
			parse = (r:Range) => Range(NOW,Time(2011,04,27))
			guess = (r:Range) => Range(Time(2011,04,26),Time(2011,04,27))
			U.sumDiff(parse.fnDiff(guess,ground)) should be (0)
			guess = (r:Range) => Range(NOW,Time(2011,04,27))
			U.sumDiff(parse.fnDiff(guess,ground)) should be (0)
			guess = (r:Range) => Range(NOW,Time(2011,04,28))
			U.sumDiff(parse.fnDiff(guess,ground)) should be (60.0*60*24)
			//(functional)
			parse = (r:Range) => Range(r.begin,Time(2011,04,27))
			guess = (r:Range) => Range(r.begin,Time(2011,04,27))
			U.sumDiff(parse.fnDiff(guess,ground)) should be (0)
			guess = (r:Range) => Range(NOW,r.end)
			U.sumDiff(parse.fnDiff(guess,ground)) should be (Integer.MAX_VALUE)
			guess = (r:Range) => r
			U.sumDiff(parse.fnDiff(guess,ground)) should be (Integer.MAX_VALUE)
		}
		it("works for function / range"){
			var parse:Parse = (r:Range) => Range(Time(2011,04,26),Time(2011,04,27))
			var guess = Range(Time(2011,04,26),Time(2011,04,27))
			U.sumDiff(parse.rangeDiff(guess,ground)) should be (0.0)
			parse = (r:Range) => Range(NOW,Time(2011,04,27))
			guess = Range(Time(2011,04,26),Time(2011,04,27))
			U.sumDiff(parse.rangeDiff(guess,ground)) should be (0.0)
			parse = (r:Range) => Range(r.begin,Time(2011,04,27))
			guess = Range(Time(2011,04,26),Time(2011,04,27))
			U.sumDiff(parse.rangeDiff(guess,ground)) should be (Integer.MAX_VALUE)
		}
	}

}




