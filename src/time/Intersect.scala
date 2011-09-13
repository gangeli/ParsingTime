package time

import scala.util.Random





case class Term(b:Long,e:Long) extends Intersectable {
	override def begin:Long = b
	override def end:Long = e
	override def toString:String = "["+begin+","+end+"]"
}

class RepeatedTerm(norm:Long,jump:Long,begin:Long
		) extends ProvidesIntersectables[Term] {
	override def has(offset:Long):Boolean = {
		try{ Thread.sleep(1); } catch { case _ => }
		true
	}
	override def intersectable(offset:Long):Term = {
		try{ Thread.sleep(1); } catch { case _ => }
		Term(begin+jump*offset,begin+jump*offset+norm)
	}
	override def toString:String = ""+norm + " every " + jump
}

object RepeatedTerm {
	def apply():RepeatedTerm
		= new RepeatedTerm(Random.nextInt(10),Random.nextInt(10000000),0L)
}




//------------------------------------------------------------------------------
// INTERSECT FUNCTIONALITY
//------------------------------------------------------------------------------
trait Intersectable {
	def begin:Long
	def end:Long
}

trait ProvidesIntersectables[A <: Intersectable] {
	def has(offset:Long):Boolean
	def intersectable(offset:Long):A
}

object Intersect {
	def intersect[A <: Intersectable](
			sourceA:ProvidesIntersectables[A],
			sourceB:ProvidesIntersectables[A]):Iterator[(Long,Long)] = {
		//--Pruning State
		var min:Long = 0
		var max:Long = 0
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
			override def children:List[SearchState] = {
				ensureTerms
				//--Invalid State
				if(  (dir > 0 && cachedA.begin < max && cachedB.begin < max) ||
				     (dir < 0 && cachedA.end   > min && cachedB.end   > min)    ) {
					return List[SearchState]()
				}
				//--Propose Child
				def propose(aI:Long,bI:Long,theStep:Long,dir:Int,moving:Symbol
						):TermSearchState = {
					//(target exists)
					val exists = sourceA.has(aI) && sourceB.has(bI) && theStep > 0
					//(moving in impossible direction)
					val isImpossible = moving match {
							case 'A =>
								if(dir > 0){ cachedA.begin > cachedB.end }
								else if(dir < 0){ cachedA.end < cachedB.begin }
								else { false }
							case 'B =>
								if(dir > 0){ cachedB.begin > cachedA.end }
								else if(dir < 0){ cachedB.end < cachedA.begin }
								else { false }
							case _ => throw new IllegalArgumentException
						}
					if(exists && !isImpossible){
						//(create candidate)
						val cand = TermSearchState(aI,bI,theStep,dir) 
						cand.ensureTerms
						//(check if candidate jumps too far)
						val jumpedOver = moving match {
								case 'A =>
									if(dir > 0){ 
										cachedA.end < cachedB.begin &&
										cand.cachedA.begin > cand.cachedB.end
									} else if(dir < 0) {
										cachedA.begin > cachedB.end &&
										cand.cachedA.end < cand.cachedB.begin
									} else { false }
								case 'B =>
									if(dir > 0){ 
										cachedB.end < cachedA.begin &&
										cand.cachedB.begin > cand.cachedA.end 
									} else if(dir < 0){ 
										cachedB.begin > cachedA.end &&
										cachedB.end < cachedA.begin 
									} else { false }
								case _ => throw new IllegalArgumentException
							}
						if(jumpedOver){
							null
						} else {
							cand // finally, it's ok!
						}
					} else {
						null
					}
				}
				//--Create Children
				var lst = List[TermSearchState]()
				if(dir >= 0){
					//(forwards)
					lst = propose(a+1,b,1L,1,'A) :: lst
					lst = propose(a+(step*2),b,step*2,1,'A) :: lst
					lst = propose(a,b+1,1L,1,'B) :: lst
					lst = propose(a,b+(step*2),step*2,1,'B) :: lst
				}
				if(dir <= 0){
					//(backward)
					lst = propose(a-1,b,1L,-1,'A) :: lst
					lst = propose(a-(step*2),b,step*2,-1,'A) :: lst
					lst = propose(a,b-1,1L,-1,'B) :: lst
					lst = propose(a,b-(step*2),step*2,-1,'B) :: lst
				}
				//--Return
				lst.filter{ _ != null }
			}
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
					cachedB.begin < cachedA.end &&
					cachedA.end <= cachedB.end
				val aTailsB:Boolean =
					cachedB.begin <= cachedA.begin &&
					cachedA.begin < cachedB.end &&
					cachedB.end <= cachedA.end
				val isEnd = aInB || bInA || bTailsA || aTailsB
				//(update cache)
				if(isEnd){
					var posCand:Long = 0
					if(posCand< cachedA.begin){ posCand = cachedA.begin }
					if(posCand< cachedB.begin){ posCand = cachedB.begin }
					var negCand:Long = 0
					if(0-negCand < 0-cachedA.end){ negCand = cachedA.end }
					if(0-negCand < 0-cachedB.end){ negCand = cachedB.end }
					min = math.min(min,negCand)
					max = math.max(max,posCand)
				}
				//(return)
				isEnd
			}
			override def cost:Double = {
				//(ensure terms)
				ensureTerms
				//(get offset)
				val offset:Long = math.max(
					//((distance from origin to A))
					if(cachedA.begin > 0) { cachedA.begin }
					else if(cachedA.end < 0){ -cachedA.end }
					else { 0L },
					//((distance from origin to B))
					if(cachedB.begin > 0) { cachedB.begin }
					else if(cachedB.end < 0){ -cachedB.end }
					else { 0L })
				//(get difference)
				val difference:Long = if(isEndState) { 0L }
					else if(cachedA.end < cachedB.begin){ cachedB.begin - cachedA.end }
					else if(cachedB.end < cachedA.begin){ cachedA.begin - cachedB.end }
					else { 0L }
				//(return score)
				(difference+offset).toDouble
			}
			override def assertEnqueueable:Boolean = {
				step > 0
			}
			override def assertDequeueable:Boolean = {
				true
			}
			override def toString:String = {
				ensureTerms
				"{"+a+","+b+":"+step+","+
				{if(dir > 0) " -> " else if(dir < 0) " <- " else " <-> "}+"}"
			}
			override def hashCode:Int = (a ^ b).toInt
			override def equals(o:Any):Boolean = {
				o match {
					case (s:TermSearchState) => s.a == a && s.b == b
					case _ => false
				}
			}
		}
		//--Iterable
		Search[TermSearchState](Search.UNIFORM_COST)
			.iterable(TermSearchState(0L,0L,1L,0)).iterator
			.map{ case (state:TermSearchState,count:Int) => (state.a, state.b) }
	}

	def main(args:Array[String]) = {
		def inter(a:Term,b:Term):Term = {
			new Term(math.max(a.begin,b.begin),math.min(a.end,b.end))
		}
		val sourceA:RepeatedTerm = new RepeatedTerm(315569260L,3155692600L,-3155692600L/2)
		val sourceB:RepeatedTerm = new RepeatedTerm(86400L,86400L,0)
//		val sourceA:RepeatedTerm = RepeatedTerm()
//		val sourceB:RepeatedTerm = RepeatedTerm()
		println("Source A: " + sourceA)
		println("Source B: " + sourceB)
		intersect(sourceA,sourceB).slice(0,100)
				.foreach{ case (a:Long,b:Long) =>
			val vA = sourceA.intersectable(a)
			val vB = sourceB.intersectable(b)
			println("Match: " + a + " and " + b + "     :: " + inter(vA,vB))
			println("  " + vA + " " + vB)
		}
		println("DONE")
	}
}
