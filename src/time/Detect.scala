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
case class DetectedTime(begin:Int,end:Int,time:Option[Temporal])
trait TimeDetector {
	def findTimes(sent:TimeSent):Array[DetectedTime]
}

//------------------------------------------------------------------------------
// FEATURES
//------------------------------------------------------------------------------
class TRIPSFeatures extends FeatureFactory[CoreMap] {
	import CRFDetector.InputAnnotation
	def sent(info:PaddedList[CoreMap],pos:Int):Option[(TimeSent,Int)] = {
		val x = info.get(pos).get[(TimeSent,Int),InputAnnotation](
			classOf[InputAnnotation])
		if(x == null) {
			None
		} else {
			Some(x)
		}
	}
	def word(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match{
			case Some((s:TimeSent,i:Int)) => U.w2str(s.words(i))
			case None => "✇"
		}
	}
	def pos(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match{
			case Some((s:TimeSent,i:Int)) => U.pos2str(s.pos(i))
			case None => "✇"
		}
	}
	
	def shape(info:PaddedList[CoreMap],pos:Int):String = {
		sent(info,pos) match{
			case Some((s:TimeSent,i:Int)) => s.shape(i)
			case None => "✇"
		}
	}

	override def getCliqueFeatures(
			info:PaddedList[CoreMap],position:Int,clique:Clique
			):java.util.Collection[String] = {
		val feats = new HashSet[String]
		(clique.maxLeft to clique.maxRight).foreach{ (relativePos:Int) =>
			val absolutePos = position+relativePos
			//(word)
			feats.add("word@"+relativePos+"="+word(info,absolutePos))
			//(pos)
			feats.add("pos@"+relativePos+"="+pos(info,absolutePos))
			//(shape)
			feats.add("shape@"+relativePos+"="+shape(info,absolutePos))
		}
		return feats
	}
}

//------------------------------------------------------------------------------
// CRF
//------------------------------------------------------------------------------
class CRFDetector(
		impl:CRFClassifier[CoreMap],
		parser:CKYParser
		) extends TimeDetector {
	import CRFDetector._

	def getTemporal(sent:TimeSent):Option[Temporal] = {
		(0 to 5).foreach{ (beamPow:Int) =>
			//(parse)
			val beam = math.pow(2,beamPow).toInt
			val parses = parser.parse(sent,beam)
			//(find correct)
			val evaluated:Array[Any] = parses
				.map{ _.evaluate }
				.dropWhile{ _.isInstanceOf[NoTime] }
			//(return)
			if(evaluated.length > 0){
				return Some(evaluated(0).asInstanceOf[Temporal])
			}
		}
		return None
	}

	override def findTimes(sent:TimeSent):Array[DetectedTime] = {
		//--Make Input
		val input:JList[CoreMap] = (0 until sent.length).map{ (i:Int) =>
			//(make word)
			val word = new CoreLabel(1)
			//(set input)
			word.set(classOf[InputAnnotation], (sent,i))
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
				val bound = (last,index)
				val temporal = None //TODO getTemporal(sent.slice(last,index))
				(DetectedTime(last,index,temporal) :: lst, -1)
			} else {
				throw new IllegalStateException("Impossible case")
			}
		}
		//--Return
		return rtn.toArray
	}
}

object CRFDetector {
	def TIME:String = "X"
	def NOTM:String = "-"

	class InputAnnotation extends CoreAnnotation[(TimeSent,Int)]{
		def getType:Class[(TimeSent,Int)] = classOf[(TimeSent,Int)]
	}

	def apply(data:DataStore[DetectionDatum]):CRFDetector = {
		//--Parser
		startTrack("Parser")
		log("initializing grammar")
		Grammar.init(G.wordIndexer)
		//TODO actually load a parser
		val parser:CKYParser = CKYParser(G.W,Grammar.RULES)
		endTrack("Parser")

		//--Create Flags
		startTrack("CRF")
		startTrack("Flags")
		val flags = new SeqClassifierFlags
		flags.featureFactory = O.crfFeatureFactory
		log(FORCE,"flags.featureFactory: " + flags.featureFactory)
		flags.backgroundSymbol = NOTM
		log(FORCE,"flags.backgroundSymbol: " + flags.featureFactory)
		flags.maxLeft = 1
		log(FORCE,"flags.maxLeft: " + flags.maxLeft)
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
					word.set(classOf[InputAnnotation], (datum.sent,i))
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
		new CRFDetector(classifier,parser)
	}
}

//------------------------------------------------------------------------------
// DETECTION DATA
//------------------------------------------------------------------------------
case class DetectionDatum(
		doc:String,sent:TimeSent,
		spans:Array[(Int,Int)],times:Array[Temporal],dct:Time
		) extends Iterable[Int] {
	def getTime(i:Int):Option[Temporal] = {
		spans.zipWithIndex.foreach{ case ((begin:Int,end:Int),index:Int) =>
			if(i >= begin && i < end){
				return Some(times(index))
			}
		}
		return None
	}
	override def iterator:Iterator[Int] = (0 until sent.length).iterator
	override def toString:String = "[" + times.length + " expr]: "+sent.toString
}
	
trait DetectionData extends DataStore[DetectionDatum]

class CoreMapDetectionStore(docs:Iterable[CoreMap],theName:String
		) extends DetectionData {
	override def name:String = theName+{if(test){"-eval"}else{"-train"}}
	override def eachExample(iter:Int):Iterable[DetectionDatum] = {
		val test = this.test
		new Iterable[DetectionDatum] {
			override def iterator:Iterator[DetectionDatum]
				= docs.iterator.flatMap{ (doc:CoreMap) =>
					//--Variables
					val dct:Time = Time(new DateTime(
						doc.get[Calendar,CalendarAnnotation](classOf[CalendarAnnotation])
						.getTimeInMillis))
					val docName:String
						= docs.head.get[String,DocIDAnnotation](classOf[DocIDAnnotation])
					//--Iterate Sentences
					doc.get[JList[CoreMap],SentencesAnnotation](SENTENCES)
						.map{ (sent:CoreMap) =>
							//(sentence)
							//((get tokens))
							val tokens:Array[CoreLabel] = 
								sent.get[JList[CoreLabel],TokensAnnotation](
									classOf[TokensAnnotation])
								.toArray.map{ _.asInstanceOf[CoreLabel] }
							//((get POS))
							val pos = tokens.map{ (lbl:CoreLabel) => 
									if(test) U.str2posTest(lbl.tag) else U.str2pos(lbl.tag) 
								}
							//((get numbers))
							val (ordinality,nums) = tokens.map{ DataLib.number(_) }.unzip
							//((get words))
							val words = tokens.zip(ordinality).map{ 
									case (lbl:CoreLabel,numType:NumberType.Value) =>
								if(numType != NumberType.NONE){ 
									G.NUM 
								} else { 
									if(test){
										U.str2wTest(lbl.word,false) 
									} else {
										U.str2w(lbl.word,false) 
									}
								}
							}
							//((create sentence))
							val timeSent = TimeSent(words,pos,nums.toArray,ordinality.toArray)
							//(spans/times)
							val exprs
								= sent.get[JList[CoreMap],TimeExpressionsAnnotation](
									classOf[TimeExpressionsAnnotation])
							val (spans,times) 
								= {if(exprs == null) Nil else asScalaBuffer(exprs)}
								.map{ (time:CoreMap) =>
									val begin:Int 
										= time.get[java.lang.Integer,BeginIndexAnnotation](
											classOf[BeginIndexAnnotation])
									val end:Int 
										= time.get[java.lang.Integer,EndIndexAnnotation](
											classOf[EndIndexAnnotation])
									val temporal = DataLib.array2JodaTime(
										time.get[Array[String],TimeValueAnnotation](
											classOf[TimeValueAnnotation]))
									((begin,end),temporal)
								}.unzip
								//(datum)
								DetectionDatum(docName,timeSent,spans.toArray,times.toArray,dct)
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
	def apply(train:O.DataInfo,eval:O.DataInfo
			):TimeData[DetectionDatum] = {
		//--Data Source
		val file:String = 
			System.getenv("HOME") + 
			"/workspace/time/aux/coremap/tempeval2-english" +
			{if(O.retokenize) "-retok" else "" } +
			{if(O.collapseNumbers) "-numbers" else "" }
		log(FORCE,"INPUT: "+file)
		//--Create Data
		val data = new SerializedCoreMapDataset(file)
		val rtn = TimeData(
			new CoreMapDetectionStore(data.slice(train.begin,train.end),
				train.source.toString),
			new CoreMapDetectionStore(data.slice(eval.begin,eval.end),
				eval.source.toString))
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
			val guess:Array[DetectedTime] = sys.findTimes(datum.sent)
			//(get span overlap)
			//((variables))
			val goldMask:Array[Boolean] = new Array[Boolean](datum.sent.length)
			val guessMask:Array[Boolean] = new Array[Boolean](datum.sent.length)
			//((fill masks))
			datum.spans.foreach{ case (begin:Int,end:Int) =>
				(begin until end).foreach{ (i:Int) => goldMask(i) = true }
			}
			guess.foreach{case DetectedTime(begin:Int,end:Int,time:Option[Temporal])=>
				(begin until end).foreach{ (i:Int) => 
					guessMask(i) = true 
					extentFile
						.append(datum.doc).append("\t")
						.append(""+datum.sent).append("\t")
						.append(""+i).append("\t")
						.append("timex3").append("\t")
						.append("TODO").append("\t")
						.append("1").append("\n")
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
			val overlapCount:Int = guessMask.zip(goldMask)
				.filter{ case (guess:Boolean,gold:Boolean) => guess && gold }.length
			val guessCount:Int = guessMask.filter{ _ == true }.length
			val goldCount:Int = goldMask.filter{ _ == true }.length
			detectCalc.enterRaw(overlapCount,guessCount,goldCount)
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
		forceTrack("loading dataset")
		val data = CoreMapDetectionStore(O.train,if(O.devTest) O.dev else O.test)
		endTrack("loading dataset")
		//--Train CRF
		startTrack("Training")
		val crf:CRFDetector = CRFDetector(data.train)
		endTrack("Training")
		//--Evaluate
		startTrack("Evaluate")
		val trainExtent = new PrintWriter(Execution.touch("train-extent.tab"))
		val evalExtent = new PrintWriter(Execution.touch("eval-extent.tab"))
		val trainScore = evaluate(crf,data.train,trainExtent)
		val evalScore  = evaluate(crf,data.eval,evalExtent)
		endTrack("Evaluate")
		//--Score
		startTrack("Scores")
		startTrack("Train")
		log("Precision: " + trainScore.detect.precision)
		log("Recall:    " + trainScore.detect.recall)
		log("F1:        " + trainScore.detect.F1)
		log("Accuracy   " + trainScore.accuracy)
		endTrack("Train")
		startTrack("Test")
		log("Precision: " + evalScore.detect.precision)
		log("Recall:    " + evalScore.detect.recall)
		log("F1:        " + evalScore.detect.F1)
		log("Accuracy   " + evalScore.accuracy)
		endTrack("Test")
		endTrack("Scores")
	}
}
