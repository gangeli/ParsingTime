package time

import scala.collection.JavaConversions._
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashSet

import Lex._
import Conversions._
import ParseConversions._

import org.goobs.exec.Log._
import org.goobs.exec.Execution
import org.goobs.slib.Def

import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.stats.Counters;

//------------------------------------------------------------------------------
// TOY PARSERS
//------------------------------------------------------------------------------

class ItsAlwaysFriday extends StandardParser{
	override def parse(i:Int, sent:Sentence, feedback:Boolean, sid:Int
			):(Array[Parse],Feedback=>Any)={
		val parse:Array[Parse] = Array[Parse](
			FRI(NOW),                              // I think it's friday
			(r:Range) => Range(r.begin,NOW),       // or 'the past'
			WEEK,                                  // or a week
			Range(Time(2011,4,26),Time(2011,4,27)) // or April 26
			)
		(parse, (feedback:Feedback) => {})
	}
}



class PrimitivesOnly extends StandardParser{
	val simplePrimitives:Array[Parse] = Array[Parse](
		NOW,
		MON(NOW),TUE(NOW),WED(NOW),THU(NOW),FRI(NOW),SAT(NOW),SUN(NOW),
		SEC,MIN,HOUR,DAY,WEEK,MONTH,YEAR
	)
	case class Feature(unigram:Int)
	
	val weights:Counter[(Feature,Int)] = new ClassicCounter[(Feature,Int)]


	override def parse(i:Int, sent:Sentence, feedback:Boolean, sid:Int
			):(Array[Parse],Feedback=>Any)={
		assert(sent != null, "Sentence cannot be null")
		//--Features
		def features(sent:Sentence,out:Int):Counter[(Feature,Int)] = {
			val counts = new ClassicCounter[(Feature,Int)]
			sent.foreach((w:Int,t:Int)=>counts.incrementCount((Feature(w),out), 1.0))
			counts
		}
		def globalIndex(i:Int,p:Array[Parse]):Int = {
			simplePrimitives.zipWithIndex.foreach( (pair) => {
				val (parse,index) = pair
				if(parse == p(i)){ return index }
			})
			throw fail("Broke equality somewhere")
		}
		//--Score Sentence
		val parse:Array[Parse] = simplePrimitives.zipWithIndex.map( pair => {
				val (p,i) = pair
				val feats = features(sent,i)
				val score = Counters.dotProduct(feats,weights)
				(score,i)
			}).sortBy(_._1).reverse.map( (pair:(Double,Int)) => {
				simplePrimitives(pair._2)
			})
		//--Update
		val update = (feedback:Feedback) => {
			if(feedback.hasCorrect){
				val feats = features(sent,globalIndex(feedback.bestIndex,parse))
				weights.addAll(feats)
			}
		}
		//--Return
		(parse, update)
	}
}

//------------------------------------------------------------------------------
// SEARCH PARSER
//------------------------------------------------------------------------------

object SearchParser {
	import Grammar._
//-----
// INTERACTIVE
//-----
	def interactive:Unit = {
		var cont:Boolean = true
		while(cont){
			val line = Console.readLine("search> ")
			println(line)
		}
	}
//-----
// FEATURES
//-----
	trait Feature
	case class UnigramLexFeature(w:Int,lex:Int) extends Feature {
		override def toString:String 
			= ""+{if(lex<0) "NIL_INTRO" else RULES(lex).toString}+"["+U.w2str(w)+"]"+
				":("+lex+","+w+")"
	}
	case class IndicatorRuleFeature(rule:Int) extends Feature {
		override def toString:String = RULES(rule).signature
	}
}


class SearchParser extends StandardParser {
	import SearchParser._
	import scala.math.{min,max,log => ln,exp}
	import Grammar._
//-----
// BEHAVIOR
//-----
	protected val weights:Counter[Feature] = new ClassicCounter[Feature]

	def ruleFeatures(state:State,rindex:Int):Counter[Feature] = {
		val feats:Counter[Feature] = new ClassicCounter
		feats.incrementCount(IndicatorRuleFeature(rindex), 1.0)
		feats
	}
	def lexFeatures(s:Sentence,i:Int,lexIndex:Int):Counter[Feature] = {
		val feats:Counter[Feature] = new ClassicCounter
		feats.incrementCount(UnigramLexFeature(s.words(i),lexIndex), 1.0)
		feats
	}

	def feedback(feats:Counter[Feature],good:Boolean,score:Double):Unit = {
		if(good){ 
			val posCounts:Counter[Feature] = new ClassicCounter[Feature]
			feats.keySet.foreach( (f:Feature) => {
				posCounts.incrementCount(f,score*feats.getCount(f))
			})
			weights.addAll(posCounts)
		} else {
			val negCounts:Counter[Feature] = new ClassicCounter[Feature]
			feats.keySet.foreach( (f:Feature) => {
				negCounts.incrementCount(f,-score*feats.getCount(f))
			})
			weights.addAll(negCounts)
		}
	}
	def score(feats:Counter[Feature]):Double = {
//		-U.safeLn(1.0 / (1.0 + exp(-Counters.dotProduct(feats,weights))))
		-Counters.dotProduct(feats,weights)
	}


//-----
// SEARCH STATE
//-----
	case class State(
			begin:Int,end:Int,
			parse:(Head.Value,Any),
			leftOf:(Head.Value,Any),rightOf:(Head.Value,Any),
			c:Double,
			updates:List[(Boolean,Double)=>Unit],
			sent:Sentence
				) extends SearchState with Ordered[State]{
		
		def realParse:Parse = Parse(parse)

		override def isEndState = {
			begin <= 0 && end >= sent.length && parse._1 == Head.ROOT
//			if(begin <= 0 && end >= sent.length){
//				parse._1 match{
//					case Head.Time => true
//					case Head.Range => true
//					case Head.Duration => true
//					case Head.F_R => true
//					case _ => false
//				}
//			} else {
//				false
//			}
		}

		override def children:List[State] = {
			var children = List[State]()
			val started = parse != null
			//--Start
			if(!started){
				for( index <- 0 until RULES.length ){ //for each rule...
					val rule:Rule = RULES(index)
					if(rule.arity == 1 && rule.accepts(Head.Word)){ //..that's lexical
						for(wordI <- 0 until sent.length){
							val feats = lexFeatures(sent,wordI,index)
							val up = feedback(feats,_:Boolean,_:Double)
							children = State(
								wordI, wordI+1,
								(rule.head, rule(sent(wordI))),
								null,null,
								score(feats),
								List[(Boolean,Double)=>Unit](up),
								sent) :: children
						}
					}
				}
			}
			//--Tag
			//(tag left)
			if(started && begin > 0 && leftOf == null){
				for( index <- 0 until RULES.length ){
					val rule:Rule = RULES(index)
					if(rule.arity == 1 && rule.accepts(Head.Word)){
						//(add rule)
						val feats = lexFeatures(sent,begin-1,index)
						val up = feedback(feats,_:Boolean,_:Double)
						children = this.copy(
							leftOf=(rule.head, rule(sent(begin-1))), 
							c=c+score(feats),
							updates=up::updates
							) :: children
					}
				}
			}
			//(tag right)
			if(started && end < sent.length-1 && rightOf == null){
				for( index <- 0 until RULES.length ){
					val rule:Rule = RULES(index)
					if(rule.arity == 1 && rule.accepts(Head.Word)){
						//(add rule)
						val feats = lexFeatures(sent,end+1,index)
						val up = feedback(feats,_:Boolean,_:Double)
						children = this.copy(
							rightOf=(rule.head, rule(sent(end+1))), 
							c=c+score(feats),
							updates=up::updates
							) :: children
					}
				}
			}
			//--Rules
			if(started){
				for( index <- 0 until RULES.length ){
					val rule:Rule = RULES(index)
					if( rule.arity == 1 && rule.accepts(parse._1) ){
						//(type raise)
						val feats = ruleFeatures(this,index)
						val up = feedback(feats,_:Boolean,_:Double)
						children = this.copy(
								parse=(rule.head,rule(parse._2)),
								c=c+score(feats),
								updates=up::updates
								) :: children
					}else if(rule.arity == 2){
						assert(begin>0 || leftOf==null, "moved too far left")
						assert(end< sent.length || rightOf==null, "moved too far right")
						val (parseType, parseValue) = parse
						//(left apply)
						if(leftOf != null){
							val (nodeType, nodeValue) = leftOf
							if(rule.accepts(nodeType, parseType)){
								val feats = ruleFeatures(this,index)
								val up = feedback(feats,_:Boolean,_:Double)
								children = this.copy(
										parse=(rule.head,rule(nodeValue, parseValue)),
										c=c+score(feats),
										updates=up::updates,
										leftOf=null,
										begin=begin-1
										) :: children
							}
						}
						//(right apply)
						if(rightOf != null){
							val (nodeType, nodeValue) = rightOf
							if(rule.accepts(parseType, nodeType)){
								val feats = ruleFeatures(this,index)
								val up = feedback(feats,_:Boolean,_:Double)
								children = this.copy(
										parse=(rule.head,rule(parseValue, nodeValue)),
										c=c+score(feats),
										updates=up::updates,
										rightOf=null,
										end=end+1
										) :: children
							}
						}
					} else if(rule.arity > 2 || rule.arity < 1){
						//(strange rule...)
						throw fail("Arity > 2 rule")
					}
				}
			}
			//--Nil Introduction
			if(started && leftOf == null && begin > 0){
				val feats = lexFeatures(sent,begin-1,-1)
				val up = feedback(feats,_:Boolean,_:Double)
				children = this.copy(
					begin=begin-1,
					c=c+score(feats),
					updates=up::updates
					) :: children
			}
			if(started && rightOf == null && end < sent.length-1){
				val feats = lexFeatures(sent,end+1,-1)
				val up = feedback(feats,_:Boolean,_:Double)
				children = this.copy(
					end=end+1,
					c=c+score(feats),
					updates=up::updates
					) :: children
			}
			//--Return
			children
		}
		override def cost:Double = c
		override def assertDequeueable:Boolean = {
			true
		}
		override def assertEnqueueable:Boolean = {
			true
		}
		override def compare(s:State) = {
			if(this.cost > s.cost){        1 }
			else if(this.cost < s.cost){  -1 }
			else{                          0 }
		}
		override def toString:String = {
			if(parse == null){
				"START"
			} else {
				""+G.df.format(cost)+" ["+begin+"-"+end+")("+parse._1+")"+parse._2
			}
		}
	}
	
	object State {
		def start(sent:Sentence):State =
			State(-1,-1,null,null,(null,null),0.0,List[(Boolean,Double)=>Unit](),sent)
	}


//-----
// PARSE
//-----
	override def report:Unit = {
		val weightQueue = Counters.toPriorityQueue(weights)
		val writer = new java.io.FileWriter(Execution.touch("weights"))
		//--Print+File
		start_track("top weights")
		for(i <- 1 to min(10,weightQueue.size)){
			val priority = weightQueue.getPriority
			val msg = "["+priority+"] " + weightQueue.removeFirst
			logG(msg)
			writer.write(msg); writer.write("\n")
		}
		//--File
		logG("" + weightQueue.size + " more...")
		for(i <- 1 to weightQueue.size){
			val priority = weightQueue.getPriority
			val msg = "["+priority+"] " + weightQueue.removeFirst
			writer.write(msg); writer.write("\n")
		}
		end_track
		writer.close
	}

	// -- Parse --
	override def parse(i:Int, sent:Sentence, feedback:Boolean, sid:Int
			):(Array[Parse],Feedback=>Any)={
		import Search._
		//--Parse
		var parseLst:List[State] = List[State]()
		val search:Search[State] = Search(memcap(UNIFORM_COST,O.beam*2,0.5))
		search.search(
			State.start(sent),
			(parse:State,iter:Int) => {
				parseLst = parse :: parseLst
				true
			},
			O.maxSearchTime)
		val parses:Array[State] = parseLst.sortWith(_<_).toArray
		//--Update (perceptron)
		val update = (fb:Feedback) => {
			if(fb.wasWrong && fb.correctCount > 0){
				val guess = parses(0)
				//(increment gold parses)
				parses(fb.correct(0)._1).updates.foreach(_(true,1.0))
//				fb.correct.foreach( (pair:(Int,Double)) => {
//					val (index,score) = pair
//					val gold = parses(index)
//					gold.updates.foreach(_(true,1.0/fb.correctCountDbl))
//				})
				//(decrement guess parse)
				guess.updates.foreach(_(false,1.0))
			}
		}
		//--Debug
		log("Parsed \"" + U.sent2str(sent.words) + "\" ("+parses.length+") as " + 
			U.join(
				parses.slice(0,1).map(
					p => ""+p.realParse+"["+G.df.format(p.cost)+"]"), " or "))
		(parses.map( _.realParse ), if(feedback){ update } else { fb=>{} } )
	}
}


