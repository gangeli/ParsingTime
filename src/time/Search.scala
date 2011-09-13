package time

import scala.concurrent.Lock
import scala.collection.mutable._

class SearchException(s:String,e:Throwable) extends RuntimeException(s,e) {
	def this() = this("",null)
	def this(s:String) = this(s,null)
	def this(e:Throwable) = this(null,e)
}

trait SearchState{
	//(minimal override)
	def children:List[SearchState]
	//(suggested overrides)
	def isEndState:Boolean = children.length == 0
	def cost:Double = 0.0
	def heuristic:Double = 0.0
	//(debug overrides)
	def assertEnqueueable:Boolean = true
	def assertDequeueable:Boolean = true
}

class Search[S <: SearchState : Manifest](store:Search.Store[S]) {
	val storeLock = new Lock
	val SINK = new SearchState{
		override def children = List[SearchState]()
		override def isEndState = true
		override def cost = java.lang.Double.POSITIVE_INFINITY
		override def heuristic:Double = 0
	}

	//TODO this method is nasty; also storeLock is never released
	def iterable(start:S,timeout:Int):Iterable[(S,Int)] = new Iterable[(S,Int)] {
		override def iterator:Iterator[(S,Int)] = new Iterator[(S,Int)] {
			//--Iterator State
			private var nxt:(S,Int) = null
			private var nextReady = false
			//--Search State
			//(startup)
			storeLock.acquire
			store.clear
			store.enqueue(start)
			//(initialize loop)
			private var shouldContinue:Boolean = true
			private var count = 0
			//--Search
			private def findNext:(S,Int) = {
				while(shouldContinue && !store.isEmpty) {
					//(get head)
					val node:S = store.dequeue
					assert(node.assertDequeueable, ""+node+" is not dequeuable")
					count += 1
					//(timeout)
					if(count >= timeout){ shouldContinue = false }
					//(add children)
					if(shouldContinue){
						node.children.foreach( (s:SearchState) => {
							try{
								assert(s.assertEnqueueable, ""+s+" is not pushable")
								store.enqueue(s.asInstanceOf[S]) //add
							} catch {
								case (ex:ClassCastException) => 
									throw new SearchException(
										"Child has bad type: " + s.getClass())
							}
						})
					}
					//(return head if ok)
					if(node.isEndState){ 
						return (node, count) 
					}
				}
				return null
			}
			//--Iterator Methods
			override def hasNext:Boolean = {
				if(nextReady){ nxt != null }
				else{ 
					nxt = findNext 
					nextReady = true
					nxt != null 
				}
			}
			override def next:(S,Int) = {
				if(!hasNext){ throw new NoSuchElementException }
				else{ nextReady = false; nxt }
			}
		}
	}
	def iterable(start:S):Iterable[(S,Int)] = iterable(start,Int.MaxValue)

	def search(
			start:S,
			result:(S,Int)=>Boolean,
			timeout:Int):Int = {
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
			val node:S = store.dequeue
			assert(node.assertDequeueable, ""+node+" is not dequeuable")
			count += 1
			//(return head if ok)
			if(node.isEndState){ shouldContinue = result(node, count) }
			//(timeout)
			if(count >= timeout){ shouldContinue = false }
			//(add children)
			if(shouldContinue){
				node.children.foreach( (s:SearchState) => {
					try{
						assert(s.assertEnqueueable, ""+s+" is not pushable")
						store.enqueue(s.asInstanceOf[S]) //add
					} catch {
						case (ex:ClassCastException) => 
							throw new SearchException("Child has bad type: " + s.getClass())
					}
				})
			}
		}
		//--Cleanup
		//(release store)
		storeLock.release
		//(return)
		count
	}
	
	def search(start:S,result:(S,Int)=>Boolean):Int
		= search(start,result,Int.MaxValue)
	def search(start:S):Array[S] = {
		var results = List[S]()
		search(start, (result:S,count:Int) => {
					results = result :: results
					true
				})
		results.reverse.toArray
	}
	def search(start:S,maxSize:Int):Array[S] = {
		var results = List[S]()
		search(start, (result:S,count:Int) => {
					results = result :: results
					results.length < maxSize
				})
		results.reverse.toArray
	}
	def best(start:S):S = {
		var rtn:S = null.asInstanceOf[S]
		search(start, (result:S,count:Int) => {rtn = result; false})
		if(rtn == null){
			throw new SearchException("No solution")
		}
		rtn
	}
}



object Search {
	type Store[S <: SearchState] = {
		def enqueue(s:S*):Unit
		def dequeue():S
		def clear():Unit
		def isEmpty:Boolean
	}

	def apply[S <: SearchState : Manifest](s:Store[S]) = new Search[S](s)

	def cache[S <: SearchState](store:Store[S]):Store[S] = {
		new {
			val cache:Set[S] = new HashSet[S]
			var next:S = null.asInstanceOf[S]
			def enqueue(s:S*):Unit = { store.enqueue(s:_*) }
			def dequeue():S = {
				if(isEmpty) {
					throw new NoSuchElementException
				}
				val rtn = next
				next = null.asInstanceOf[S]
				cache += rtn
				rtn
			}
			def clear():Unit = { cache.clear; store.clear }
			def isEmpty:Boolean = {
				if(next != null){ return false } //have a next
				if(store.isEmpty){ return true } //underlying collection empty
				var cand:S = store.dequeue 
				while(cache contains (cand)){  //find an element not in cache
					if(store.isEmpty){ return true }
					cand = store.dequeue
				}
				next = cand //set that element
				false
			}
		}
	}

	def memcap[S <: SearchState](
			store:Store[S], trigger:Int=200000, factor:Double=0.5):Store[S] = {
		//--Argument Check
		if(trigger <= 0){throw new IllegalArgumentException("Invalid trigger val")}
		if(factor >= 1.0 || factor <= 0.0){
			throw new IllegalArgumentException("Invalid reduction factor")
		}
		//--Returned Store
		new {
			var size:Int = 0
			def enqueue(s:S*):Unit = { 
				//(enqueue)
				store.enqueue(s:_*)
				size += s.length
				//(check for memory overflow)
				if(size >= trigger){
					//(get first elements)
					var lst = List[S]()
					for(i <- 1 to (trigger*factor).asInstanceOf[Int]){
						lst = store.dequeue :: lst
					}
					//(reset store)
					clear()
					enqueue(lst.reverse:_*)
				}
			}
			def dequeue():S = {size-=1; store.dequeue}
			def clear():Unit = {store.clear; size=0}
			def isEmpty:Boolean = {assert(!store.isEmpty||size==0,""); store.isEmpty}
		}
	}

	def DFS[S <: SearchState]:Store[S] = new scala.collection.mutable.Stack[S]{
			def enqueue(s:S*):Unit = pushAll(s.reverse)
			def dequeue():S = pop()
		}
	def BFS[S <: SearchState]:Store[S] = new scala.collection.mutable.Queue[S]
	def UNIFORM_COST[S <: SearchState]:Store[S] = 
		new PriorityQueue[S]()(
			new Ordering[S]{
				def compare(a:S,b:S) = {
					if(a.cost < b.cost){ 1 }
					else if(a.cost > b.cost){ -1 }
					else { 0 }
				}
			})
	def A_STAR[S <: SearchState]:Store[S] =
		new PriorityQueue[S]()(
			new Ordering[S]{
				def compare(a:S,b:S) = {
					if((a.cost+a.heuristic) < (b.cost+b.heuristic)){ 1 }
					else if((a.cost+a.heuristic) > (b.cost+b.heuristic)){ -1 }
					else { 0 }
				}
			})
}




