package time

//(java)
import java.util.{List => JList}
import java.util.Calendar
import java.io.File
import java.io.PrintWriter
//(scala)
import scala.collection.JavaConversions._
import scala.collection.mutable.HashSet
//(stanford)
import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.util.PaddedList
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreAnnotation
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.time.TimeAnnotations._
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.sequences.SeqClassifierFlags
import edu.stanford.nlp.sequences.FeatureFactory
import edu.stanford.nlp.sequences.Clique
//(jodatime)
import org.joda.time.DateTimeZone
import org.joda.time.DateTime
//(lib)
import org.goobs.stanford.JavaNLP._
import org.goobs.stanford.SerializedCoreMapDataset
import org.goobs.nlp.CKYParser
import org.goobs.nlp.NodeType
import org.goobs.nlp.NodeTypeFactory
import org.goobs.testing.ScoreCalc
import org.goobs.exec.Execution

//------------------------------------------------------------------------------
// DETECTION SYSTEM
//------------------------------------------------------------------------------
case class DetectedTime(begin:Int,end:Int,time:Option[(Temporal,Temporal,Double)]) {
	var meta:Option[(String,Int)] = None
	def tagMetaInfo(doc:String,sentI:Int):DetectedTime
		= tagMetaInfo( Some((doc,sentI)) )
	def tagMetaInfo(meta:Option[(String,Int)]):DetectedTime = {
		this.meta = meta
		this
	}
}
trait TimeDetector {
	def findTimes(sent:TimeSent,parse:TimeSent=>Option[(Temporal,Temporal,Double)],
			chart:CRFDetector.Chart,gloss:Array[String]):Array[DetectedTime]
	
}


//------------------------------------------------------------------------------
// FEATURES
//------------------------------------------------------------------------------
object CRFFeatures {
	val triggers = Set[String](
		"monday","tuesday","wednesday","thursday","friday","saturday","sunday",
		"mon","tue","wed","thu","fri","sat","sun",
		"january","february","march","april","may","june","july","august",
			"september","october","november","december",
		"jan","feb","mar","jun","jul","aug","sep","aug","nov","dec",
		"second","minute","hour","day","week","month","quarter","year","decade","century",
		"seconds","minutes","hours","days","weeks","months","quarters","years","decades","centuries",
		"winter","spring","summer","fall",
		"today","tomorrow","yesterday",
		"now"
	)
}
@SerialVersionUID(1L)
class CRFFeaturesWithTriggers(index:Indexing, ckyTriggers:Set[String]
		)extends CRFFeatures(index,ckyTriggers) {
	import CRFFeatures.triggers
	
	override def getCliqueFeatures(
			info:PaddedList[CoreMap],position:Int,clique:Clique
			):java.util.Collection[String] = {
		//--Super Features
		val feats = super.getCliqueFeatures(info,position,clique)
		//--Triggers
		if(clique == FeatureFactory.cliqueC){
			if(triggers.contains(word(info,position).toLowerCase)){
				feats.add("trigger")
			}
		}
		//--Return
		return feats
	}
}

@SerialVersionUID(1L)
class CRFFeatures(index:Indexing,triggers:Set[String]) extends FeatureFactory[CoreMap] {
	import CRFDetector.InputAnnotation
	import CRFDetector.Chart

	def sent(info:PaddedList[CoreMap],pos:Int):Option[(TimeSent,Array[String],Chart,Int)] = {
		val x = info.get(pos).get[(TimeSent,Array[String],Chart,Int),InputAnnotation](
			classOf[InputAnnotation])
		if(x == null) {
			None
		} else {
			Some(x)
		}
	}
	def chartProb(info:PaddedList[CoreMap],begin:Int,end:Int):Double = {
		assert(end > begin)
		val (sent,gloss,chart,i) = 
			info.get(0).get[(TimeSent,Array[String],Chart,Int),InputAnnotation](classOf[InputAnnotation])
		val node = chart.keys.iterator.next._1
		val key = (node, math.max(0,begin), math.min(info.size,end))
		if(chart.contains(key)){
			chart( key )
		} else {
			log("missing key: " + key + " spawned from " + begin + " until " + end)
			-50
		}
	}
	def word(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match{
			case Some((s:TimeSent,gloss:Array[String],chart:Chart,i:Int)) => 
				if(s(i) == index.NUM){
					s.nums(i).toString
				} else {
					gloss(i)
				}
			case None => "✇"
		}
	}
	def lemma(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match{
			case Some((s:TimeSent,gloss:Array[String],chart:Chart,i:Int)) => 
				if(s(i) == index.NUM){
					"--num--"
				} else {
					G.morphology.lemma(gloss(i), index.pos2str(s.pos(i))).toLowerCase
				}
			case None => "✇"
		}
	}
	def pos(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match{
			case Some((s:TimeSent,gloss:Array[String],chart:Chart,i:Int)) => index.pos2str(s.pos(i))
			case None => "✇"
		}
	}
	def shape(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match{
			case Some((s:TimeSent,gloss:Array[String],chart:Chart,i:Int)) => 
				U.shape(gloss(i))
			case None => "✇"
		}
	}
	def characterizeNumber(info:PaddedList[CoreMap],pos:Int):(Int,String,String) = {
		//returns: number, ordinality, magnitude
		sent(info,pos) match {
			case Some((s:TimeSent,gloss:Array[String],chart:Chart,i:Int)) =>
				if(s(i) == index.NUM){
					( s.nums(i), 
					  s.ordinality(i).toString, 
						s.nums(i).toString.length.toString)
				} else {
					throw new IllegalArgumentException("Not a number: " + s.gloss(i))
				}
			case None => (-1,"✇","✇")
		}
	}
	def isNumber(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match {
			case Some((s:TimeSent,gloss:Array[String],chart:Chart,i:Int)) =>
				if(s(i) == index.NUM){
					"true"
				} else {
					"false"
				}
			case None => "✇"
		}
	}


	override def getCliqueFeatures(
			info:PaddedList[CoreMap],position:Int,clique:Clique
			):java.util.Collection[String] = {
		val feats = new HashSet[String]
		//--Helpers
		def addFeature(str:String){
			feats.add(str)
		}
		def localFeatures(relativePos:Int){
			val absolutePos = position+relativePos
			val dir:String = {
				if(relativePos < 0){ "neg" }
				else if(relativePos == 0){ "0" }
				else{ "pos" }
			}
			addFeature("word@" +relativePos+"="+word(info,absolutePos))
			addFeature("lemma@"+relativePos+"="+lemma(info,absolutePos))
			addFeature("trigger@"+relativePos+"="+triggers.contains(word(info,position).toLowerCase))
		}
		def characterNgrams(word:String,n:Int,relativePosition:Int){
			val chars = word.toCharArray
			(1 to n).foreach{ (len:Int) =>
				((-len+1) until chars.length).foreach{ (start:Int) =>
					//(get n-gram)
					val gram = (start until (start+len)).foldLeft(""){ case (soFar:String,i:Int) =>
						if(i < 0 || i >= chars.length){
							soFar + "✇"
						} else {
							soFar + chars(i)
						}
					}
					//(add feature)
					addFeature("ngram@"+relativePosition+"="+gram)
				}
			}
		}
		def prefix(n:Int) = word(info,position).slice(0,n)
		def suffix(n:Int) = {
			val str = word(info,position)
			str.slice(math.max(0,str.length-n),str.length)
		}
		//--Features
		//(general surrounding features)
		(-O.maxLookaround to O.maxLookaround).foreach{ (position:Int) =>
			localFeatures(position)
		}
		if(clique == FeatureFactory.cliqueC){
			//(trigger n-gram)
			addFeature("trigger@"+0+"="+triggers.contains(word(info,position).toLowerCase))
			//(affixes)
			(1 to 5).foreach{ (i:Int) =>
				addFeature("prefix("+i+")@"+0+"="+prefix(i))
				addFeature("suffix("+i+")@"+0+"="+suffix(i))
			}
			//(affix shapes)
			(1 to 5).foreach{ (i:Int) =>
				addFeature("prefixShape(i)@"+0+"="+U.shape(prefix(i)))
				addFeature("suffixShape(i)@"+0+"="+U.shape(suffix(i)))
			}
			//(pos)
			addFeature("POS@"+0+"="+pos(info,position))
			//(shape)
			addFeature("shape@"+0+"="+shape(info,position))
			//(isnumber)
			addFeature("isnumber@"+0+"="+isNumber(info,position))
			//(surrounding number stuff)
			if(isNumber(info,position) == "true"){
				val (num,numType,numMag) = characterizeNumber(info,position)
				addFeature("numberType@"+0+"="+numType)
				addFeature("numberMagnitude@"+0+"="+numMag)
				addFeature("number@"+0+"=|"+numType+"|*10^"+numMag)
			}
		} else if(clique == FeatureFactory.cliqueCpC){
			//(triggers)
			if(triggers.contains(word(info,position-1).toLowerCase)){
				addFeature("trigger->"+word(info,position))
			}
			if(triggers.contains(word(info,position+1).toLowerCase)){
				addFeature(word(info,position)+"->trigger")
			}
//			//(chart prob)
//			log(G.df.format((chartProb(info,position,position+1)))+"  "+word(info,position))
//			if(position > 0){
//				addFeature("lprob_-1_0="+
//					((chartProb(info,position-1,position)/2).toInt*2)+","+
//					((chartProb(info,position-1,position)/2).toInt*2))
//			}
//			addFeature("lprob_0="+((chartProb(info,position,position+1)/2).toInt*2))
//			addFeature("lprob_-1+0="+((chartProb(info,position-1,position+1)/2).toInt*2))
		}
		return feats
	}
}

//------------------------------------------------------------------------------
// CRF
//------------------------------------------------------------------------------
@SerialVersionUID(1L)
case class CRFDetector(impl:CRFClassifier[CoreMap],index:Indexing
		) extends TimeDetector {
	import CRFDetector._


	def findTimes(sent:TimeSent,
			parse:TimeSent=>Option[(Temporal,Temporal,Double)],
			chart:Chart,
			gloss:Array[String]
				):Array[DetectedTime] = {
		//--Make Input
		val input:JList[CoreMap] = (0 until sent.length).map{ (i:Int) =>
			//(make word)
			val word = new CoreLabel(1)
			//(set input)
			word.set(classOf[InputAnnotation], (sent,gloss,chart,i))
			//(return)
			word
		}
		//--Classify
		val output = impl.classify(input) //output eq input
		//--Convert Output
		//(get annotations)
		val isTime:List[Boolean] = output
			.map{ _.get[String,AnswerAnnotation](classOf[AnswerAnnotation]) }
			.map{ x => assert(x != null); x == TIME }
			.toList ::: List[Boolean](true) //padded for last time case
		//(collect annotations)
		val (rtn,jun) = 
			isTime.zipWithIndex.foldLeft(List[DetectedTime](),-1){
				case (
					(lst:List[DetectedTime],last:Int),
					(isTime:Boolean,index:Int)
					) => 
			if(isTime && last == -1){
				//(case: enterred a time)
				(lst,index)
			} else if(isTime){
				//(case: in a time)
				(lst,last)
			} else if(!isTime && last == -1){
				//(case: out of a time)
				(lst,-1)
			} else if(!isTime) {
				//(case: left a time)
				val temporal =  parse(sent.slice(last,index))
				(DetectedTime(last,index,temporal) :: lst, -1)
			} else {
				throw new IllegalStateException("Impossible case")
			}
		}
		//--Return
		return rtn.map{ _.tagMetaInfo(sent.meta) }.reverse.toArray
	}
	
	def corelabels2timeSent(tokens:Array[CoreLabel]):TimeSent = {
		//(get POS)
		val pos = tokens.map{ (lbl:CoreLabel) => index.str2posTest(lbl.tag) }
		//(get numbers)
		val (ordinality,nums) = tokens.map{ DataLib.number(_) }.unzip
		//(get words)
		val words = tokens.zip(ordinality).map{ 
				case (lbl:CoreLabel,numType:NumberType.Value) =>
			if(numType != NumberType.NONE){ 
				index.NUM 
			} else { 
					index.str2wTest(lbl.word,false) 
			}
		}
		//(create sentence)
		TimeSent(words,pos,nums.toArray,ordinality.toArray,index)
	}

}

object CRFDetector {
	val TIME:String = "X"
	val NOTM:String = "-"
	
	type Chart = Map[(NodeType,Int,Int),Double]

	class InputAnnotation extends CoreAnnotation[(TimeSent,Array[String],Chart,Int)]{
		def getType:Class[(TimeSent,Array[String],Chart,Int)] 
			= classOf[(TimeSent,Array[String],Chart,Int)]
	}

	def apply(data:DataStore[DetectionDatum],index:Indexing,treeTime:TreeTime):CRFDetector = {
		//--Parser
		startTrack("Parser")
		log("loading TreeTime")
		val parser:CKYParser = treeTime.parser
		val grammar:Grammar = treeTime.grammar
		val interpretIndexer = treeTime.index
		val lex:Lex = treeTime.lex
		log("calculating common lex terms")
		val triggers:Set[String] = grammar.lexWords(parser)
		endTrack("Parser")

		//--Create Flags
		startTrack("CRF")
		startTrack("Flags")
		val flags = new SeqClassifierFlags
		flags.featureFactory = O.crfFeatureFactory
		log(FORCE,"flags.featureFactory: " + flags.featureFactory)
		flags.featureFactoryArgs = Array[java.lang.Object](index, triggers)
		log(FORCE,"flags.featureFactoryArgs: " + 
			"Object["+flags.featureFactoryArgs.length+"]")
		flags.backgroundSymbol = NOTM
		log(FORCE,"flags.backgroundSymbol: " + flags.featureFactory)
		flags.maxLeft = 1
		log(FORCE,"flags.maxLeft: " + flags.maxLeft)
		flags.maxRight = 0
		log(FORCE,"flags.maxRight: " + flags.maxRight)
		flags.sigma = O.crfSigma
		log(FORCE,"flags.sigma: " + flags.sigma)
		endTrack("Flags")
		val classifier = new CRFClassifier[CoreMap](flags)
		//--Create Data
		startTrack("Data")
		val javaData:JList[JList[CoreMap]] = seqAsJavaList(
			data.eachExample(Int.MaxValue).zipWithIndex.map{ case 
					(datum:DetectionDatum,index:Int) =>
				val b:StringBuilder = new StringBuilder
				//(parse)
				//(set)
				val lst:JList[CoreMap] = seqAsJavaList( datum.map{ (i:Int) =>
					//(make word)
					val word = new CoreLabel(2)
					//(set input)
					word.set(classOf[InputAnnotation], (datum.sent,datum.sent.toGlossArray,datum.chart,i))
					//(set output)
					word.set(classOf[AnswerAnnotation],datum.getTime(i) match {
						case Some(temporal) => b.append(TIME); TIME
						case None => b.append(NOTM); NOTM
					})
					//(return)
					word
				}.toList)
				log("sentence " + index +": " + b.toString)
				lst
			}.toList)
		endTrack("Data")
		//--Train
		startTrack("Train")
		classifier.train(javaData)
		endTrack("Train")
		forceTrack("Params")
		val f = Execution.touch("crf-weights.txt")
		classifier.writeWeights(new java.io.PrintStream(f))
		endTrack("Params")
		endTrack("CRF")

		//--Return
		new CRFDetector(classifier,index)
	}

}

//------------------------------------------------------------------------------
// DETECTION DATA
//------------------------------------------------------------------------------
case class DetectionDatum(
		doc:String,sent:TimeSent,sentIndex:Int,
		spans:Array[(Int,Int)],times:Array[Temporal],dct:Time,
		gloss:Array[String],chart:CRFDetector.Chart
		) extends Iterable[Int] {
	assert(gloss.length == sent.length, "Gloss length mismatch")

	def getTime(i:Int):Option[Temporal] = {
		spans.zipWithIndex.foreach{ case ((begin:Int,end:Int),index:Int) =>
			if(i >= begin && i < end){
				return Some(times(index))
			}
		}
		return None
	}
	var origBegin:Int=>Int = (i:Int) => i
	def setOrigBegin(orig:Int=>Int):DetectionDatum = {
		origBegin = orig
		this
	}
	var origEnd:Int=>Int = (i:Int) => i
	def setOrigEnd(orig:Int=>Int):DetectionDatum = {
		origEnd = orig
		this
	}
	var tids:Option[Array[String]] = None
	def setTids(t:Array[String]):DetectionDatum = {
		tids = Some(t)
		this
	}
	override def iterator:Iterator[Int] = (0 until sent.length).iterator
	override def toString:String = "[" + times.length + " expr]: "+sent.toString
}
	
trait DetectionData extends DataStore[DetectionDatum]

@SerialVersionUID(2L)
class CoreMapDetectionStore(docs:Iterable[CoreMap],theName:String,val index:Indexing,
		getChart:TimeSent=>CRFDetector.Chart
		) extends DetectionData with Serializable {
	
	var impl:Option[Array[DetectionDatum]] = None

	override def name:String = theName+{if(test){"-eval"}else{"-train"}}
	override def eachExample(iter:Int):Iterable[DetectionDatum] = {
		impl match {
			case Some(data) => data
			case None => 
				val test = this.test
				val iterable = new Iterable[DetectionDatum] {
					override def iterator:Iterator[DetectionDatum]
						= docs.iterator.flatMap{ (doc:CoreMap) =>
							//--Process Document
							DataLib.retokenize(doc)
							DataLib.normalizeNumbers(doc)
							//--Variables
							val dct:Time = Time(new DateTime(
								doc.get[Calendar,CalendarAnnotation](classOf[CalendarAnnotation])
								.getTimeInMillis))
							val docName:String
								= doc.get[String,DocIDAnnotation](classOf[DocIDAnnotation])
							//--Iterate Sentences
							doc.get[JList[CoreMap],SentencesAnnotation](SENTENCES)
								.zipWithIndex
								.map{ case (sent:CoreMap,sentI:Int) =>
									//(sentence)
									//((get tokens))
									val tokens:Array[CoreLabel] = 
										sent.get[JList[CoreLabel],TokensAnnotation](
											classOf[TokensAnnotation])
										.toArray.map{ _.asInstanceOf[CoreLabel] }
									//((get POS))
									val pos = tokens.map{ (lbl:CoreLabel) => 
											if(test) index.str2posTest(lbl.tag) else index.str2pos(lbl.tag) 
										}
									//((get numbers))
									val (ordinality,nums) = tokens.map{ DataLib.number(_) }.unzip
									//((get words))
									val words = tokens.zip(ordinality).map{ 
											case (lbl:CoreLabel,numType:NumberType.Value) =>
										if(numType != NumberType.NONE){ 
											index.NUM 
										} else { 
											if(test){
												index.str2wTest(lbl.word,false) 
											} else {
												index.str2w(lbl.word,false) 
											}
										}
									}
									//((get gloss))
									val gloss = tokens.map{ _.word }
									//((create sentence))
									val timeSent = 
										TimeSent(words,pos,nums.toArray,ordinality.toArray,index)
										.tagMetaInfo(docName,sentI)
									//(spans/times)
									val exprs
										= sent.get[JList[CoreMap],TimeExpressionsAnnotation](
											classOf[TimeExpressionsAnnotation])
									val (theSpans,theTemporals) 
										= {if(exprs == null) Nil else asScalaBuffer(exprs)}
										.map{ (time:CoreMap) =>
											//((span))
											val begin:Int 
												= time.get[java.lang.Integer,BeginIndexAnnotation](
													classOf[BeginIndexAnnotation])
											val end:Int 
												= time.get[java.lang.Integer,EndIndexAnnotation](
													classOf[EndIndexAnnotation])
											//((original span))
											val origBegin:Int 
												= time.get[java.lang.Integer,OriginalBeginIndexAnnotation](
													classOf[OriginalBeginIndexAnnotation])
											val origEnd:Int 
												= time.get[java.lang.Integer,OriginalEndIndexAnnotation](
													classOf[OriginalEndIndexAnnotation])
											//((temporal value))
											val temporal = DataLib.array2JodaTime(
												time.get[Array[String],TimeValueAnnotation](
													classOf[TimeValueAnnotation]))
											//((tid))
											val tid = 
												time.get[String,TimeIdentifierAnnotation](
													classOf[TimeIdentifierAnnotation])
											(((begin,end),(origBegin,origEnd)),(temporal,tid))
										}	.sortBy{ _._2._2.substring(1).toInt } //sort by tid
											.unzip
										val (spans,origSpans) = theSpans.unzip
										val (times,tids) = theTemporals.unzip
										//(chart)
										val chart = getChart(timeSent)
										//(datum)
										DetectionDatum(
											docName,timeSent,sentI,
											spans.toArray,times.toArray,dct,
											gloss,chart
											)
											.setOrigBegin({ (i:Int) =>
												tokens(i).get[java.lang.Integer,TokenBeginAnnotation](
													classOf[TokenBeginAnnotation])
											})
											.setOrigEnd({ (i:Int) =>
												tokens(i).get[java.lang.Integer,TokenEndAnnotation](
													classOf[TokenEndAnnotation])
											})
											.setTids(tids.toArray)
								}
						}
				}
				this.impl = Some(iterable.toArray)
				this.impl.get
		}
	}
	def test:Boolean = {
		assert(docs.size > 0, "Dataset is empty")
		val rtn = docs.iterator.next
			.get[Boolean,IsTestAnnotation](classOf[IsTestAnnotation])
		assert(docs.forall{
				_.get[Boolean,IsTestAnnotation](classOf[IsTestAnnotation]) == rtn
			}, "Dataset is not homogeneous")
		rtn
	}
}

object CoreMapDetectionStore {
	def getChart(treeTime:TreeTime) = (s:TimeSent) => {
		Map[(NodeType,Int,Int),Double]()
//		val sent = s.reIndex(treeTime.index,3)
//		log(FORCE,"chart |sent|=" + sent.length)
//		val chart = treeTime.parser.chart(sent,0,sent.length,List(treeTime.grammar.factory.ROOT))
//		//--Check
//		(0 until sent.length).foreach{ (begin:Int) =>
//			(begin+1 to sent.length).foreach{ (end:Int) =>
//				assert(chart.contains( (treeTime.grammar.factory.ROOT, begin, end)),
//					"No match for " + begin + ", " + end + " in sentence " + sent)
//			}
//		}
//		//--Return
//		chart
	}

	def apply(train:O.DataInfo,eval:O.DataInfo,index:Indexing,
			treeTime:TreeTime
			):TimeData[DetectionDatum] = {
    throw new IllegalStateException("Fix local paths")
		//--Data Source
		val file:String = 
			System.getenv("HOME") + 
			"/workspace/time/aux/coremap/tempeval2-english"
//			{if(O.retokenize) "-retok" else "" } +
//			{if(O.collapseNumbers) "-numbers" else "" }
		log(FORCE,"INPUT: "+file)
		//--Create Data
		val data = new SerializedCoreMapDataset(file)
		val rtn = TimeData(
			new CoreMapDetectionStore(data.slice(train.begin,train.end),
				train.source.toString,index,getChart(treeTime)),
			new CoreMapDetectionStore(data.slice(eval.begin,eval.end),
				eval.source.toString,index,getChart(treeTime))
			)
		//--NOOP Loop
		startTrack("NOOP loop")
		rtn.noopLoop{ log(_) }
		endTrack("NOOP loop")
		//--Return
		rtn
	}
}


//------------------------------------------------------------------------------
// DETECTION TASK
//------------------------------------------------------------------------------
class DetectionTask extends TemporalTask {
	case class DetectionScore(detect:ScoreCalc[Boolean],accuracy:Double)

	def evaluate(sys:TimeDetector,data:DataStore[DetectionDatum],
			extentFile:PrintWriter
			):DetectionScore = {
		forceTrack(data.name)
		//--Variables
		val detectCalc = new ScoreCalc[Boolean]
		//--Loop
		data.eachExample(Int.MaxValue).zipWithIndex.foreach{ 
				case (datum:DetectionDatum,index:Int) =>
			//(make prediction)
			val guess:Array[DetectedTime] 
				= sys.findTimes(datum.sent,(t:TimeSent)=>None,datum.chart,datum.gloss)
			//(get span overlap)
			//((variables))
			val goldMask:Array[Boolean] = new Array[Boolean](datum.sent.length)
			val guessMask:Array[Boolean] = new Array[Boolean](datum.sent.length)
			//((fill masks))
			datum.spans.zipWithIndex.foreach{ case ((begin:Int,end:Int),s:Int) =>
				(begin until end).foreach{ (i:Int) => 
					goldMask(i) = true 
				}
			}
			guess.zipWithIndex.foreach{ case (tm:DetectedTime,tmI:Int) =>
				(tm.begin until tm.end).foreach{ (i:Int) => 
					guessMask(i) = true 
				}
				tm.meta.foreach{ case (doc:String,sentI:Int) =>
					(datum.origBegin(tm.begin) until datum.origEnd(tm.end-1))
							.foreach{ (i:Int) =>
						extentFile
							.append(doc).append("\t")                  //document name
							.append(""+sentI).append("\t")             //sentence index
							.append(""+i).append("\t")                 //word index
							.append("timex3").append("\t")             // <junk>
							.append("t"+(1000*index+tmI)).append("\t") //unique timex
							.append("1").append("\n")                  // <junk>
					}
				}
			}
			//((debug log))
			if(!goldMask.zip(guessMask).forall{ case (a,b) => a == b }){
				log(FORCE,"sentence " + index +": " + 
					goldMask.zip(guessMask).zip(datum.gloss)
					.map{ case ((a,b),word) => 
						if(a && b) "✔" 
						else if(!a && !b) "-" 
						else if(a) "[R:"+word+"]" 
						else "[P:"+word+"]" }
					.mkString(""))
			} else {
				log("sentence " + index + " (correct)")
			}
			//((get counts))
			goldMask.zip(guessMask).foreach{ case (gold,guess) => 
				if(guess && gold){
					detectCalc.enterDiscrete(true,true)
				} else if(gold){
					detectCalc.recallMiss(true)
				} else if(guess){
					detectCalc.precisionMiss(true)
				}
			}
			//(get temporal correctness)
			//TODO do this
		}
		endTrack(data.name)
		DetectionScore(detectCalc,0.0) //TODO accuracy score
	}

	override def run:Unit = {
		//--Initialize JodaTime
		log("JodaTime settings")
		DateTimeZone.setDefault(DateTimeZone.UTC);
		//--Create Data
		forceTrack("Training Interpretation Model")
		val sys:TreeTime = if(O.runInterpretModel){
			(new InterpretationTask).run
			IOUtils.readObjectFromFileNoExceptions(Execution.touch("interpretModel.ser.gz"))
		} else {
			IOUtils.readObjectFromFileNoExceptions(O.interpretModel)
		}
		endTrack("Training Interpretation Model")
		forceTrack("Loading serialized dataset")
		val (data, index) = if(new File("aux/detectData.ser.gz").exists){
				log(FORCE,"Loading data")
				IOUtils.readObjectFromFile("aux/detectData.ser.gz").asInstanceOf[(TimeData[DetectionDatum],Indexing)]
			} else {
				val index = Indexing()
				val data = CoreMapDetectionStore(O.train,if(O.devTest) O.dev else O.test,index,sys)
				IOUtils.writeObjectToFile((data,index), "aux/detectData.ser.gz")
				(data,index)
			}
		endTrack("Loading serialized dataset")
		//--Train CRF
		startTrack("Training")
		val crf:CRFDetector = CRFDetector(data.train,index,sys)
		log("augmenting TreeTime")
		val detectSys:TreeTime = sys.addDetector(crf,index)
		log("saving interpretation model")
		IOUtils.writeObjectToFile(sys,Execution.touch("interpretModel.ser.gz"))
		log("saving detection model")
		IOUtils.writeObjectToFile(detectSys,Execution.touch("treeTime.ser.gz"))
		endTrack("Training")
		//--Evaluate
		startTrack("Evaluate")
		val trainExtent = new PrintWriter(Execution.touch("train-extent.tab"))
		val evalExtent = new PrintWriter(Execution.touch("eval-extent.tab"))
		val trainScore = evaluate(crf,data.train,trainExtent)
		val evalScore  = evaluate(crf,data.eval,evalExtent)
		trainExtent.close
		evalExtent.close
		endTrack("Evaluate")
		//--Score
		val logger = Execution.getLogger();
		startTrack("Scores")
		startTrack("Train")
		log("Precision: " + trainScore.detect.precision)
		log("Recall:    " + trainScore.detect.recall)
		log(YELLOW,"F1:        " + trainScore.detect.F1)
		log("Accuracy   " + trainScore.accuracy)
		logger.setGlobalResult("detect.train.precision", trainScore.detect.precision)
		logger.setGlobalResult("detect.train.recall",    trainScore.detect.recall)
		logger.setGlobalResult("detect.train.f1",        trainScore.detect.F1)
		logger.setGlobalResult("detect.train.accuracy",  trainScore.accuracy)
		endTrack("Train")
		startTrack("Test")
		log("Precision: " + evalScore.detect.precision)
		log("Recall:    " + evalScore.detect.recall)
		log(YELLOW,"F1:        " + evalScore.detect.F1)
		log("Accuracy   " + evalScore.accuracy)
		logger.setGlobalResult("detect.eval.precision", evalScore.detect.precision)
		logger.setGlobalResult("detect.eval.recall",    evalScore.detect.recall)
		logger.setGlobalResult("detect.eval.f1",        evalScore.detect.F1)
		logger.setGlobalResult("detect.eval.accuracy",  evalScore.accuracy)
		endTrack("Test")
		endTrack("Scores")
	}
}
