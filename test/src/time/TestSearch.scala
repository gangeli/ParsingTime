package time

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import time._

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
	val node2 = node("node2", List[SearchState]()           , 5, 0)
	val node4 = node("node4", List[SearchState]()           , 8, 1)
	val node6 = node("node6", List[SearchState]()           , 6, 0)
	val node5 = node("node5", List[SearchState](node6)      , 5, 0)
	val node3 = node("node3", List[SearchState](node5)      , 4, 1)
	val node1 = node("node1", List[SearchState](node4,node3), 1, 3)
	val node0 = node("node0", List[SearchState](node2,node1), 5, 4)
}


class SearchSpec extends Spec with ShouldMatchers{
	describe("DFS") {
		it("should be creatable"){ 
			val dfs:Search = Search.dfs
		}
		it("works for Fake Tree"){
			val dfs:Search = Search.dfs
			val results:Array[SearchState] = dfs.search(FakeTree.node0)
			results.length should be (3)
			results(0) should be (FakeTree.node6)
			results(1) should be (FakeTree.node4)
			results(2) should be (FakeTree.node2)
			
		}
	}
	describe("BFS") {
		it("should be creatable"){ 
			val bfs:Search = Search.bfs
		}
		it("works for Fake Tree"){
			val bfs:Search = Search.bfs
			val results:Array[SearchState] = bfs.search(FakeTree.node0)
			results.length should be (3)
			results(0) should be (FakeTree.node2)
			results(1) should be (FakeTree.node4)
			results(2) should be (FakeTree.node6)
			
		}
	}
	describe("Uniform Cost Search") {
		it("should be creatable"){ 
			val ucs:Search = Search.uniformCost
		}
		it("works for Fake Tree"){
			val ucs:Search = Search.uniformCost
			val results:Array[SearchState] = ucs.search(FakeTree.node0)
			results.length should be (3)
			results(0) should be (FakeTree.node2)
			results(1) should be (FakeTree.node6)
			results(2) should be (FakeTree.node4)
			ucs.search(FakeTree.node0, (s,c)=>false) should be (4)
		}
	}
	describe("AStar") {
		it("should be creatable"){ 
			val astar:Search = Search.astar
		}
		it("works for Fake Tree"){
			val astar:Search = Search.astar
			val results:Array[SearchState] = astar.search(FakeTree.node0)
			results.length should be (3)
			results(0) should be (FakeTree.node2)
			results(1) should be (FakeTree.node6)
			results(2) should be (FakeTree.node4)
			astar.search(FakeTree.node0, (s,c)=>false) should be (3) //pruning
		}
	}
}





