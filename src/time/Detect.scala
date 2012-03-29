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
			gloss:Array[String]):Array[DetectedTime]
	
	def findTimes(sent:TimeSent):Array[DetectedTime] 
		= findTimes(sent, (t:TimeSent) => None, sent.toGlossArray)
}

//------------------------------------------------------------------------------
// FEATURES
//------------------------------------------------------------------------------
@SerialVersionUID(1L)
class TRIPSFeatures(index:Indexing) extends FeatureFactory[CoreMap] {
	import CRFDetector.InputAnnotation
	def sent(info:PaddedList[CoreMap],pos:Int):Option[(TimeSent,Array[String],Int)] = {
		val x = info.get(pos).get[(TimeSent,Array[String],Int),InputAnnotation](
			classOf[InputAnnotation])
		if(x == null) {
			None
		} else {
			Some(x)
		}
	}
	def word(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match{
			case Some((s:TimeSent,gloss:Array[String],i:Int)) => 
				if(s(i) == index.NUM){
					s.nums(i).toString
				} else {
					gloss(i)
				}
			case None => "✇"
		}
	}
	def pos(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match{
			case Some((s:TimeSent,gloss:Array[String],i:Int)) => index.pos2str(s.pos(i))
			case None => "✇"
		}
	}
	def shape(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match{
			case Some((s:TimeSent,gloss:Array[String],i:Int)) => 
				U.shape(gloss(i))
			case None => "✇"
		}
	}
	def characterizeNumber(info:PaddedList[CoreMap],pos:Int):(Int,String,String) = {
		//returns: number, ordinality, magnitude
		sent(info,pos) match {
			case Some((s:TimeSent,gloss:Array[String],i:Int)) =>
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
			case Some((s:TimeSent,gloss:Array[String],i:Int)) =>
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
			addFeature("word@"+dir+"="+word(info,absolutePos))
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
		if(clique == FeatureFactory.cliqueC){
			//(general surrounding features)
			(-O.maxLookaround to O.maxLookaround).foreach{ (position:Int) =>
				localFeatures(position)
			}
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
				addFeature("wordBeforeNum="+word(info,position-1))
				addFeature("wordAfterNum="+word(info,position+1))
				addFeature("numberType@"+0+"="+numType)
				addFeature("numberMagnitude@"+0+"="+numMag)
				addFeature("number@"+0+"=|"+numType+"|*10^"+numMag)
			}
		} else {
			(-O.maxLookaround to O.maxLookaround).foreach{ (position:Int) =>
				localFeatures(position)
			}
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


	def findTimes(sent:TimeSent,parse:TimeSent=>Option[(Temporal,Temporal,Double)],
			gloss:Array[String]
				):Array[DetectedTime] = {
		//--Make Input
		val input:JList[CoreMap] = (0 until sent.length).map{ (i:Int) =>
			//(make word)
			val word = new CoreLabel(1)
			//(set input)
			word.set(classOf[InputAnnotation], (sent,gloss,i))
			//(return)
			word
		}
		//--Classify
		CRFDetector.parser = Some(parse) //TODO thread safety here
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
	
	var parser:Option[TimeSent=>Option[(Temporal,Temporal,Double)]] = None

	class InputAnnotation extends CoreAnnotation[(TimeSent,Array[String],Int)]{
		def getType:Class[(TimeSent,Array[String],Int)] 
			= classOf[(TimeSent,Array[String],Int)]
	}

	def apply(data:DataStore[DetectionDatum],index:Indexing,treeTime:TreeTime):CRFDetector = {
		//--Parser
		startTrack("Parser")
		log("loading TreeTime")
		val parser:CKYParser = treeTime.parser
		val interpretIndexer = treeTime.index
		val lex:Lex = treeTime.lex
		endTrack("Parser")

		//--Create Flags
		startTrack("CRF")
		startTrack("Flags")
		val flags = new SeqClassifierFlags
		flags.featureFactory = O.crfFeatureFactory
		log(FORCE,"flags.featureFactory: " + flags.featureFactory)
		flags.featureFactoryArgs = Array[java.lang.Object](index)
		log(FORCE,"flags.featureFactoryArgs: " + 
			"Object["+flags.featureFactoryArgs.length+"]")
		flags.backgroundSymbol = NOTM
		log(FORCE,"flags.backgroundSymbol: " + flags.featureFactory)
		flags.maxLeft = 1
		log(FORCE,"flags.maxLeft: " + flags.maxLeft)
		flags.maxRight = 0
		log(FORCE,"flags.maxRight: " + flags.maxRight)
		endTrack("Flags")
		val classifier = new CRFClassifier[CoreMap](flags)
		//--Create Data
		startTrack("Data")
		val javaData:JList[JList[CoreMap]] = seqAsJavaList(
			data.eachExample(Int.MaxValue).zipWithIndex.map{ case 
					(datum:DetectionDatum,index:Int) =>
				val b:StringBuilder = new StringBuilder
				val lst:JList[CoreMap] = seqAsJavaList( datum.map{ (i:Int) =>
					//(make word)
					val word = new CoreLabel(2)
					//(set input)
					word.set(classOf[InputAnnotation], (datum.sent,datum.sent.toGlossArray,i))
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
		gloss:Array[String]
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

class CoreMapDetectionStore(docs:Iterable[CoreMap],theName:String,index:Indexing
		) extends DetectionData {
	override def name:String = theName+{if(test){"-eval"}else{"-train"}}
	override def eachExample(iter:Int):Iterable[DetectionDatum] = {
		val test = this.test
		new Iterable[DetectionDatum] {
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
								//(datum)
								DetectionDatum(
									docName,timeSent,sentI,
									spans.toArray,times.toArray,dct,
									gloss
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
	def apply(train:O.DataInfo,eval:O.DataInfo,index:Indexing
			):TimeData[DetectionDatum] = {
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
				train.source.toString,index),
			new CoreMapDetectionStore(data.slice(eval.begin,eval.end),
				eval.source.toString,index))
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
				= sys.findTimes(datum.sent,(t:TimeSent)=>None,datum.gloss)
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
					goldMask.zip(guessMask)
					.map{ case (a,b) => 
						if(a && b) "✔" 
						if(!a && !b) "✓" 
						else if(a) "R" 
						else "P" }
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
		forceTrack("Loading dataset")
		val index = Indexing()
		val data = CoreMapDetectionStore(O.train,if(O.devTest) O.dev else O.test,index)
		endTrack("Loading dataset")
		forceTrack("Training Interpretation Model")
		val sys:TreeTime = if(O.runInterpretModel){
			(new InterpretationTask).run
			IOUtils.readObjectFromFileNoExceptions(Execution.touch("interpretModel.ser.gz"))
		} else {
			IOUtils.readObjectFromFileNoExceptions(O.interpretModel)
		}
		endTrack("Training Interpretation Model")
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
