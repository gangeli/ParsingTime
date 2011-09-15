package time

import scala.util.Random
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap





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
// SOME UTILITIES
//------------------------------------------------------------------------------
class IteratorMap[A <: {def index:Int}](iter:Iterator[A]) extends Map[Int,A] {
	private val mapImpl:HashMap[Int,A] = new HashMap[Int,A]
	override def += (kv:(Int,A)) = {
		throw new UnsupportedOperationException("Cannot add to iterable map")
	}
	override def -= (k:Int) = {
		throw new UnsupportedOperationException("Cannot remove from iterable map")
	}
	override def get (key:Int):Option[A] = {
		while(iter.hasNext && !mapImpl.contains(key)){
			val value = iter.next
//			println("Setting " + value.index + " to " + value)
			mapImpl(value.index) = value
		}
		mapImpl.get(key)
	}
	override def iterator:Iterator[(Int,A)] = {
		(1 until Int.MaxValue).iterator.map{ case (i:Int) =>
			if(i % 2 == 0){ (i/2, apply( i/2 )) }
			else { ((i-1)/2, apply( -(i-1)/2 )) }
		}
	}
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

case class Intersection(index:Int,a:Long,b:Long,origin:(Long,Long))

object Intersect {
	private case class DistInfo(aMoved:Boolean,bMoved:Boolean,dist:Long)

	def intersect[A <: Intersectable](
			sourceA:ProvidesIntersectables[A],
			sourceB:ProvidesIntersectables[A]):Iterator[Intersection] = {
		//--Pruning State
		var min:Long = Long.MaxValue
		var max:Long = Long.MinValue
		//--Search State
		case class TermSearchState(a:Long,b:Long,step:Long,dir:Int,moving:Symbol,
				origin:Option[(Long,Long)],minDist:DistInfo) extends SearchState {
			def this(a:Long,b:Long) 
				= this(a,b,0L,0,'None,None,DistInfo(false,false,Long.MaxValue))


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
				//(already past here)
				if(  (dir > 0 && cachedA.begin < max && cachedB.begin < max) ||
				     (dir < 0 && cachedA.end   > min && cachedB.end   > min)    ) {
//					println("  (invalid " + min + " " + max + ")")
					return List[SearchState]()
				}
				//(insufficient progress)
				if(minDist.aMoved && minDist.bMoved && distanceBetween >= minDist.dist){
					return List[SearchState]()
				}
				//--Propose Child
				//(save match)
				val isMatch = this.isEndState
				//(propose function)
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
						//(create "sufficient progress" term)
						val distBetween:Long = distanceBetween
						val newDist = 
							if(isMatch){
								DistInfo(false,false,Long.MaxValue)
							} else if(distBetween < minDist.dist){
								DistInfo(false,false,distBetween)
							} else {
								DistInfo(
									minDist.aMoved || moving == 'A, 
									minDist.bMoved || moving == 'B, 
									minDist.dist)
							}
						//(create candidate)
						val cand = if(isMatch){
								origin match {
									case Some(o) => 
										TermSearchState(aI,bI,theStep,dir,moving,origin,newDist)
									case None => 
										TermSearchState(aI,bI,theStep,dir,moving,
											Some((aI,bI)), newDist)
								}
							} else {
								TermSearchState(aI,bI,theStep,dir,moving,None, newDist) 
							}
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
										cand.cachedB.end < cand.cachedA.begin 
									} else { false }
								case _ => throw new IllegalArgumentException
							}
//						if(jumpedOver){
//							println("  jumped over: " + cand)
//							null
//						} else {
							cand // finally, it's ok!
//						}
					} else {
//						println("  impossible: " + aI + " " + bI + " " + exists + " " + !isImpossible)
						null
					}
				}
				//--Create Children
				var lst = List[TermSearchState]()
				if(dir >= 0){
					//(forwards)
					lst = propose(a+1,b,1L,1,'A) :: lst
					if(moving == 'A){
						lst = propose(a+(step*2),b,step*2,1,'A) :: lst
					}
					lst = propose(a,b+1,1L,1,'B) :: lst
					if(moving == 'B){
						lst = propose(a,b+(step*2),step*2,1,'B) :: lst
					}
				}
				if(dir <= 0){
					//(backward)
					lst = propose(a-1,b,1L,-1,'A) :: lst
					if(moving == 'A){
						lst = propose(a-(step*2),b,step*2,-1,'A) :: lst
					}
					lst = propose(a,b-1,1L,-1,'B) :: lst
					if(moving == 'B){
						lst = propose(a,b-(step*2),step*2,-1,'B) :: lst
					}
				}
				//--Return
				val rtn = lst.filter{ _ != null }
//				if(dir < 0){ println("    ["  + isMatch + "] " + rtn) }
				rtn
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
					var posCand:Long = Long.MinValue
					if(cachedA.begin > posCand){ posCand = cachedA.begin }
					if(cachedB.begin > posCand){ posCand = cachedB.begin }
					var negCand:Long = Long.MaxValue
					if(cachedA.end < negCand){ negCand = cachedA.end }
					if(cachedB.end < negCand){ negCand = cachedB.end }
					if(negCand < 0){
						min = math.min(min,negCand)
					}
					if(posCand > 0){
						max = math.max(max,posCand)
					}
				}
				//(return)
				isEnd
			}
			private def distanceBetween:Long = {
				ensureTerms
				if(isEndState) { 0L }
					else if(cachedA.end < cachedB.begin){ cachedB.begin - cachedA.end }
					else if(cachedB.end < cachedA.begin){ cachedA.begin - cachedB.end }
					else { 0L }
			}
			private def distanceOffset:Long = {
				ensureTerms
				math.max(
					//((distance from origin to A))
					if(cachedA.begin > 0) { cachedA.begin }
					else if(cachedA.end < 0){ -cachedA.end }
					else { 0L },
					//((distance from origin to B))
					if(cachedB.begin > 0) { cachedB.begin }
					else if(cachedB.end < 0){ -cachedB.end }
					else { 0L })
			}
			override def cost:Double
				= (distanceBetween+distanceOffset).toDouble
			override def assertEnqueueable:Boolean = {
				step > 0
			}
			override def assertDequeueable:Boolean = {
				ensureTerms
//				if(dir < 0){ println("  " + this )}// + " " + cachedA + " " + cachedB) }
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
		var isFirst = true
		var matchesPos = 0
		var matchesNeg = 0
		Search[TermSearchState](Search.cache(Search.UNIFORM_COST))
			.iterable(new TermSearchState(0L,0L)).iterator
			.map{ case (state:TermSearchState,count:Int) => 
//				println("MATCHED " + state)
				val index 
					= if(isFirst) { isFirst = false; 0 }
					  else if(state.dir >= 0){ matchesPos += 1; matchesPos }
					  else{ matchesNeg -= 1; matchesNeg }
				state.origin match {
					case Some(o) => Intersection(index,state.a,state.b,o)
					case None => Intersection(index,state.a,state.b,(state.a,state.b))
				}
			}
	}




	def main(args:Array[String]) = {
		def inter(a:Term,b:Term):Term = {
			new Term(math.max(a.begin,b.begin),math.min(a.end,b.end))
		}
		val sourceA:RepeatedTerm = new RepeatedTerm(5, 100, 500)
		val sourceB:RepeatedTerm = new RepeatedTerm(3,50,100)
//		val sourceA:RepeatedTerm = RepeatedTerm()
//		val sourceB:RepeatedTerm = RepeatedTerm()
		println("Source A: " + sourceA)
		println("Source B: " + sourceB)
		intersect(sourceA,sourceB).slice(0,100)
				.foreach{ (info:Intersection) =>
			val vA = sourceA.intersectable(info.a)
			val vB = sourceB.intersectable(info.b)
			println("---------------------------")
			println("Match: "+info.a+" and "+info.b+"     :: "+inter(vA,vB))
			println("  " + vA + " " + vB)
			println("---------------------------")
		}
		println("DONE")
	}
}
