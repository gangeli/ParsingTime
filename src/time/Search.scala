package time

import scala.concurrent.Lock
import scala.collection.mutable._

class SearchException(s:String,e:Throwable) extends RuntimeException(s,e) {
	def this() = this("",null)
	def this(s:String) = this(s,null)
	def this(e:Throwable) = this(null,e)
}

trait SearchState {
	def children:List[SearchState]
	def isEndState:Boolean
	def hasChildren:Boolean
	def cost:Double = 0.0
	def heuristic:Double = 0.0
}

class Search(store:{
				def enqueue(s:SearchState*):Unit
				def dequeue():SearchState
				def clear():Unit
				def isEmpty:Boolean
			}) {
	val storeLock = new Lock
	val SINK = new SearchState{
		override def children = List[SearchState]()
		override def isEndState = true
		override def hasChildren = true
		override def cost = java.lang.Double.POSITIVE_INFINITY
		override def heuristic:Double = 0
	}
	
	def search(start:SearchState,result:(SearchState,Int)=>Boolean):Int = {
		//--Setup
		//(initialize store)
		storeLock.acquire
		store.clear
		store.enqueue(start)
		//(initialize loop)
		var shouldContinue:Boolean = true
		var count = 0
		//--Search
		while(shouldContinue && !store.isEmpty) {
			//(get head)
			val node:SearchState = store.dequeue
			count += 1
			//(return head if ok)
			if(node.isEndState){
				shouldContinue = result(node, count)
			}
			//(add children)
			if(shouldContinue){
				node.children.foreach( (s:SearchState) => {
					store.enqueue(s)
				})
			}
		}
		//--Cleanup
		//(release store)
		storeLock.release
		//(return)
		count
	}
	def search(start:SearchState):Array[SearchState] = {
		var results = List[SearchState]()
		search(start, (result:SearchState,count:Int) => {
					results = result :: results
					true
				})
		results.reverse.toArray
	}
	def search(start:SearchState,maxSize:Int):Array[SearchState] = {
		var results = List[SearchState]()
		search(start, (result:SearchState,count:Int) => {
					results = result :: results
					results.length < maxSize
				})
		results.reverse.toArray
	}
	def best(start:SearchState):SearchState = {
		var rtn:SearchState = null
		search(start, (result:SearchState,count:Int) => {rtn = result; false})
		if(rtn == null){
			throw new SearchException("No solution")
		}
		rtn
	}
}



object Search {
	def dfs = new Search(new scala.collection.mutable.Stack[SearchState]{
			def enqueue(s:SearchState*):Unit = pushAll(s)
			def dequeue():SearchState = pop()
		})
	def bfs = new Search(new scala.collection.mutable.Queue[SearchState])
	def uniformCost = new Search(new PriorityQueue[SearchState]()(
		new Ordering[SearchState]{
			def compare(a:SearchState,b:SearchState) = {
				if(a.cost < b.cost){ 1 }
				else if(a.cost > b.cost){ -1 }
				else { 0 }
			}
		}))
	def astar = new Search(new PriorityQueue[SearchState]()(
		new Ordering[SearchState]{
			def compare(a:SearchState,b:SearchState) = {
				if((a.cost+a.heuristic) < (b.cost+b.heuristic)){ 1 }
				else if((a.cost+a.heuristic) > (b.cost+b.heuristic)){ -1 }
				else { 0 }
			}
		}))
}




