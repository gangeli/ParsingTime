package time

import scala.util.Random

trait Intersectable {
	def begin:Long
	def end:Long
}

trait ProvidesIntersectables[A <: Intersectable] {
	def has(offset:Long):Boolean
	def intersectable(offset:Long):A
}




case class Term(b:Long,e:Long) extends Intersectable {
	override def begin:Long = b
	override def end:Long = e
	override def toString:String = "["+begin+","+end+"]"
}

class RepeatedTerm(norm:Long,jump:Long) extends ProvidesIntersectables[Term] {
	override def has(offset:Long):Boolean = true
	override def intersectable(offset:Long):Term
		= Term(jump*offset,jump*offset+norm)
	override def toString:String = ""+norm + " every " + jump
}

object RepeatedTerm {
	def apply():RepeatedTerm
		= new RepeatedTerm(Random.nextInt(10),Random.nextInt(10000000))
}





object Intersect {
	def intersect[A <: Intersectable](
			origin:Long,
			sourceA:ProvidesIntersectables[A],
			sourceB:ProvidesIntersectables[A]):Iterable[(Long,Long)] = {
		//--Search State
		case class TermSearchState(a:Long,b:Long,step:Long,dir:Int) 
				extends SearchState {
			private var cachedA:A = null.asInstanceOf[A]
			private var cachedB:A = null.asInstanceOf[A]
			private def ensureTerms:Unit = {
				if(cachedA == null){
					cachedA = sourceA.intersectable(a)
				}
				if(cachedB == null){
					cachedB = sourceB.intersectable(b)
				}
			}
			private def isValid(dir:Int,lastMoved:Symbol):Boolean = {
				ensureTerms
				lastMoved match {
					case 'A => cachedB.begin < cachedA.end
						if(dir > 0){ cachedB.begin < cachedA.end }
						else if(dir < 0){ cachedB.end > cachedA.begin }
						else { true }
					case 'B => 
						if(dir > 0){ cachedA.begin < cachedB.end }
						else if(dir < 0){ cachedA.end > cachedB.begin }
						else { true }
					case _ => throw new IllegalArgumentException
				}
			}
			override def children:List[SearchState] = {
				def propose(aI:Long,bI:Long,theStep:Long,dir:Int,moving:Symbol
						):TermSearchState = {
					if(sourceA.has(aI) && sourceB.has(bI) && theStep > 0) {
						val cand = TermSearchState(aI,bI,theStep,dir) 
						if(cand.isValid(dir,moving)){ cand } else { null }
					} else {
						null
					}
				}
				var lst = List[TermSearchState]()
				if(dir >= 0){
					//(forwards)
					lst = propose(a+step,b,step,1,'A) :: lst
					lst = propose(a+(step*2),b,step*2,1,'A) :: lst
					lst = propose(a+(step/2),b,step/2,1,'A) :: lst
					lst = propose(a,b+step,step,1,'B) :: lst
					lst = propose(a,b+(step*2),step*2,1,'B) :: lst
					lst = propose(a,b+(step/2),step/2,1,'B) :: lst
				}
				if(dir <= 0){
					//(backward)
					lst = propose(a-step,b,step,-1,'A) :: lst
					lst = propose(a-(step*2),b,step*2,-1,'A) :: lst
					lst = propose(a-(step/2),b,step/2,-1,'A) :: lst
					lst = propose(a,b-step,step,-1,'B) :: lst
					lst = propose(a,b-(step*2),step*2,-1,'B) :: lst
					lst = propose(a,b-(step/2),step/2,-1,'B) :: lst
				}
				lst.filter{ _ != null }
			}
			override def hasChildren:Boolean = children.length > 0
			override def isEndState:Boolean = {
				//(ensure terms)
				ensureTerms
				//(check intersect cases)
				val aInB:Boolean = 
					cachedA.begin >= cachedB.begin &&
					cachedA.end <= cachedB.end
				val bInA:Boolean = 
					cachedA.begin <= cachedB.begin &&
					cachedA.end >= cachedB.end
				val bTailsA:Boolean =
					cachedA.begin <= cachedB.begin &&
					cachedB.begin <= cachedA.end &&
					cachedA.end <= cachedB.end
				val aTailsB:Boolean =
					cachedB.begin <= cachedA.begin &&
					cachedA.begin <= cachedB.end &&
					cachedB.end <= cachedA.end
				//(return)
				aInB || bInA || bTailsA || aTailsB
			}
			override def cost:Double = {
				//(ensure terms)
				ensureTerms
				//(get offset)
				val offset:Long = math.max(
					//((distance from origin to A))
					if(cachedA.begin > origin) { cachedA.begin - origin }
					else if(cachedA.end < origin){ origin - cachedA.end }
					else { 0L },
					//((distance from origin to B))
					if(cachedB.begin > origin) { cachedB.begin - origin }
					else if(cachedB.end < origin){ origin - cachedB.end }
					else { 0L })
				//(get difference)
				val difference:Long = if(isEndState) { 0L }
					else if(cachedA.end < cachedB.begin){ cachedB.begin - cachedA.end }
					else if(cachedB.end < cachedA.begin){ cachedA.begin - cachedB.end }
					else { throw new IllegalStateException }
				//(return score)
				(difference+offset).toDouble
			}
			override def assertEnqueueable:Boolean = {
				step > 0
			}
			override def assertDequeueable:Boolean = {
				println(this + ": " + cost)
				true
			}
			override def toString:String = "<"+a+","+b+":"+step+">"
			override def hashCode:Int = (a ^ b).toInt
			override def equals(o:Any):Boolean = {
				o match {
					case (s:TermSearchState) => s.a == a && s.b == b
					case _ => false
				}
			}
		}
		//--Iterable
		Search[TermSearchState](Search.cache(Search.UNIFORM_COST))
			.search(TermSearchState(0L,0L,1L,0), (res:TermSearchState,count:Int) => {
					println("RESULT>>>> "+res + "  ::  " +
						sourceA.intersectable(res.a) + "  " +
						sourceB.intersectable(res.b) + "  ")
					true
				}
			)
		println("DONE-----------------------------")
		null
//		Search[TermSearchState](Search.UNIFORM_COST)
//			.iterable(TermSearchState(0L,0L,1L,0))
//			.map{ case (state:TermSearchState,count:Int) => (state.a, state.b) }
	}

	def main(args:Array[String]) = {
		val sourceA:RepeatedTerm = new RepeatedTerm(100,1000000000)
		val sourceB:RepeatedTerm = new RepeatedTerm(3,7)
//		val sourceA:RepeatedTerm = RepeatedTerm()
//		val sourceB:RepeatedTerm = RepeatedTerm()
		val origin:Long = 0L
		println("Source A: " + sourceA)
		println("Source B: " + sourceB)
		intersect(origin,sourceA,sourceB).slice(0,10)
				.foreach{ case (a:Long,b:Long) =>
			println("Match: " + a + " and " + b)
		}
		println("DONE")
	}
}
