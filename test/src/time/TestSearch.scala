package time

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import time._
import Search._

object FakeTree {
	def node(name:String, childrenLst:List[SearchState],
			s:Double=0.0,h:Double=0.0) = {
		new SearchState {
			override def children:List[SearchState] = childrenLst
			override def isEndState:Boolean = {childrenLst.length == 0}
			override def hasChildren:Boolean = {childrenLst.length > 0}
			override def cost:Double = s
			override def heuristic:Double = h
			override def toString:String = name
		}
	}
	val node2:SearchState = node("node2", List[SearchState]()           , 5, 0)
	val node4:SearchState = node("node4", List[SearchState]()           , 8, 1)
	val node6:SearchState = node("node6", List[SearchState]()           , 6, 0)
	val node5:SearchState = node("node5", List[SearchState](node6)      , 5, 0)
	val node3:SearchState = node("node3", List[SearchState](node5)      , 4, 1)
	val node1:SearchState = node("node1", List[SearchState](node4,node3), 1, 3)
	val node0:SearchState = node("node0", List[SearchState](node2,node1), 5, 4)
}

object Manhattan {
	var EDGE = java.lang.Integer.MAX_VALUE
}
class Manhattan(val x:Int,val y:Int,val c:Double) extends SearchState {
	def this(x:Int,y:Int) = this(x,y,0.0)
	import scala.math.abs
	//--Search Overrides
	override def children:List[Manhattan] 
		= List[Manhattan](
			new Manhattan(x-1,y,c+1),new Manhattan(x+1,y,c+1),
			new Manhattan(x,y-1,c+1),new Manhattan(x,y+1,c+1))
	override def isEndState:Boolean = x == 0 && y == 0
	override def hasChildren:Boolean = true
	override def cost:Double = c
	override def heuristic:Double= (abs(x)+abs(y)).asInstanceOf[Double]
	//--Debug Overrides
	override def assertDequeueable:Boolean = {
		x < Manhattan.EDGE && y < Manhattan.EDGE
	}
	//--Standard Overrides
	override def toString:String = "("+x+","+y+":"+c+")"
	override def equals(o:Any):Boolean = o match {
			case (m:Manhattan) => (m.x==x && m.y==y)
			case _ => false
		}
	override def hashCode:Int = x.hashCode ^ (y.hashCode << 16)
}


class EuclideanManhattan(x:Int,y:Int,c:Double) extends Manhattan(x,y,c) {
	def this(x:Int,y:Int) = this(x,y,0.0)
	override def children:List[EuclideanManhattan] 
		= List[EuclideanManhattan](
			new EuclideanManhattan(x-1,y,c+1),new EuclideanManhattan(x+1,y,c+1),
			new EuclideanManhattan(x,y-1,c+1),new EuclideanManhattan(x,y+1,c+1))
	override def cost:Double = c
	override def heuristic:Double= scala.math.sqrt((x*x+y*y).asInstanceOf[Double])
	//--Debug Overrides
	override def assertDequeueable:Boolean = {
		x < Manhattan.EDGE && y < Manhattan.EDGE
	}
}


class SearchSpec extends Spec with ShouldMatchers{
	val BREAK = (s:SearchState,c:Int) => false
	val IGNORE = (s:SearchState,c:Int) => true
	describe("DFS") {
		it("should be creatable"){ 
			val dfs:Search[SearchState] = Search(DFS)
		}
		it("works for Fake Tree"){
			val dfs:Search[SearchState] = Search(DFS)
			val results:Array[SearchState] = dfs.search(FakeTree.node0)
			results.length should be (3)
			results(0) should be (FakeTree.node6)
			results(1) should be (FakeTree.node4)
			results(2) should be (FakeTree.node2)
		}
		it("breaks for Manhattan"){
			val dfs:Search[Manhattan] = Search(DFS)
			val count = dfs.search(new Manhattan(5,5),BREAK,10000)
			count should be (10000)
		}
	}
	describe("BFS") {
		it("should be creatable"){ 
			val bfs:Search[SearchState] = Search(BFS)
		}
		it("works for Fake Tree"){
			val bfs:Search[SearchState] = Search(BFS)
			val results:Array[SearchState] = bfs.search(FakeTree.node0)
			results.length should be (3)
			results(0) should be (FakeTree.node2)
			results(1) should be (FakeTree.node4)
			results(2) should be (FakeTree.node6)
		}
		it("works for Manhattan"){
			val bfs:Search[Manhattan] = Search(BFS)
			//(correctness)
			val out:Manhattan = bfs.best(new Manhattan(5,5))
			out should be (new Manhattan(0,0))
			//(time)
			val count = bfs.search(new Manhattan(5,5),BREAK)
			count should be > (1000)
		}
	}
	describe("Uniform Cost Search") {
		it("should be creatable"){ 
			val ucs:Search[SearchState] = Search(UNIFORM_COST)
		}
		it("works for Fake Tree"){
			val ucs:Search[SearchState] = Search(UNIFORM_COST)
			val results:Array[SearchState] = ucs.search(FakeTree.node0)
			results.length should be (3)
			results(0) should be (FakeTree.node2)
			results(1) should be (FakeTree.node6)
			results(2) should be (FakeTree.node4)
			ucs.search(FakeTree.node0, (s,c)=>false) should be (4)
		}
		it("works for Manhattan"){
			val ucs:Search[Manhattan] = Search(UNIFORM_COST)
			//(correctness)
			val out:Manhattan = ucs.best(new Manhattan(5,5))
			out should be (new Manhattan(0,0))
			//(time)
			val count = ucs.search(new Manhattan(5,5),BREAK)
			count should be > (1000)
			//(consistency with BFS)
			val bfs:Search[Manhattan] = Search(BFS)
			val bfsCount:Int = bfs.search(new Manhattan(5,5),BREAK)
			(scala.math.abs(count - bfsCount) < (0.05*count)) should be (true)
		}
	}
	describe("AStar (uncached)") {
		it("should be creatable"){ 
			val astar:Search[SearchState] = Search(A_STAR)
		}
		it("works for Fake Tree"){
			val astar:Search[SearchState] = Search(A_STAR)
			val results:Array[SearchState] = astar.search(FakeTree.node0)
			results.length should be (3)
			results(0) should be (FakeTree.node2)
			results(1) should be (FakeTree.node6)
			results(2) should be (FakeTree.node4)
			astar.search(FakeTree.node0, (s,c)=>false) should be (3) //pruning
		}
		it("works for Manhattan"){
			val astar:Search[Manhattan] = Search(A_STAR)
			//(correctness)
			val out:Manhattan = astar.best(new Manhattan(5,5))
			out should be (new Manhattan(0,0))
			//(time)
			var count = astar.search(new Manhattan(5,5),BREAK)
			count should be < (100)
			//(mid time)
			count = astar.search(new Manhattan(10,10),BREAK)
			count should be < (2000)
		}
		it("breaks for Manhattan (large)"){
			val astar:Search[Manhattan] = Search(A_STAR)
			//(long time)
			val count = astar.search(new Manhattan(100,100),BREAK,100000)
			count should be (100000)
		}
	}
	describe("AStar (cached)"){
		it("should be creatable"){ 
			val astar:Search[SearchState] = Search(cache(A_STAR))
		}
		it("works for Fake Tree"){
			val astar:Search[SearchState] = Search(cache(A_STAR))
			val results:Array[SearchState] = astar.search(FakeTree.node0)
			results.length should be (3)
			results(0) should be (FakeTree.node2)
			results(1) should be (FakeTree.node6)
			results(2) should be (FakeTree.node4)
			astar.search(FakeTree.node0, (s,c)=>false) should be (3) //pruning
		}
		it("works for Manhattan"){
			val astar:Search[Manhattan] = Search(cache(A_STAR))
			//(correctness)
			val out:Manhattan = astar.best(new Manhattan(5,5))
			out should be (new Manhattan(0,0))
			//(time)
			var count = astar.search(new Manhattan(5,5),BREAK)
			count should be < (100)
			//(mid time)
			count = astar.search(new Manhattan(10,10),BREAK)
			count should be < (200)
		}
		it("works for Manhattan (large)"){
			val astar:Search[Manhattan] = Search(cache(A_STAR))
			//(long time)
			Manhattan.EDGE=102
			val count = astar.search(new Manhattan(100,100),BREAK)
			count should be < (10000)
			Manhattan.EDGE=java.lang.Integer.MAX_VALUE
		}
		it("works for Imprecise Manhattan"){
			val astar:Search[EuclideanManhattan] = Search(cache(A_STAR))
			//(long time)
			val count = astar.search(new EuclideanManhattan(100,100),BREAK)
			count should be < (100000)
			//(better than ucs)
			val ucs:Search[EuclideanManhattan] = Search(cache(UNIFORM_COST))
			val ucsCount = ucs.search(new EuclideanManhattan(100,100),BREAK)
			count should be < (ucsCount)
		}
	}
}





