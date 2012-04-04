package time

//(scala)
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import scala.sys.process._
//(java)
import java.util.{List => JList}
import java.lang.{Integer => JInt}
import java.text.DecimalFormat
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.Properties
import java.util.Calendar
import java.io.File
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import java.io.ObjectOutputStream
import java.io.FileWriter
//(jodatime)
import org.joda.time.DateTimeZone
import org.joda.time.DateTime
//(stanford)
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.util.logging.StanfordRedwoodConfiguration
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.time.{Timex => StanfordTimex}
import edu.stanford.nlp.time.GUTimeAnnotator
import edu.stanford.nlp.time.HeidelTimeAnnotator
import edu.stanford.nlp.time.TimeAnnotator
import edu.stanford.nlp.time.TimeAnnotations._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.process.Morphology
//(lib)
import org.goobs.exec.Execution
import org.goobs.util.Indexer
import org.goobs.util.MetaClass
import org.goobs.util.Stopwatch
import org.goobs.util.Def
import org.goobs.util.Static._
import org.goobs.stanford.SerializedCoreMapDataset
import org.goobs.stanford.CoreMapDatum
import org.goobs.stanford.StanfordExecutionLogInterface
import org.goobs.stats.CountStore
import org.goobs.nlp._



//------------------------------------------------------------------------------
// GLOBAL UTILITIES
//------------------------------------------------------------------------------
object Indexing {
	def apply():Indexing = new Indexing(new Indexer[String],new Indexer[String], "--NUM--")
}
@SerialVersionUID(1L)
case class Indexing(
		wordIndexer:Indexer[String],
		posIndexer:Indexer[String],
		numString:String
		) {
	private val HasNum = """^(.*?)([0-9]+)(.*?)$""".r
	private var frozen = false
	
	val NUM:Int = wordIndexer.addAndGetIndex("--NUM--")

	def W:Int = wordIndexer.size+1
	def P:Int = posIndexer.size+1
	def UNK:Int = W-1
	def PUNK:Int = P-1
	

	def w2str(w:Int):String = {
		if(w < wordIndexer.size){
			wordIndexer.get(w) 
		} else {
			"--UNK--"
		}
	}
	def str2w(str:String,matchNum:Boolean=true):Int = {
		if(frozen){ throw fail("Indexer is now immutable") }
		(str,matchNum) match {
			case (HasNum(prefix,num,suffix),true) => NUM
			case _ => 
				if(O.ignoreCase) {
					wordIndexer.addAndGetIndex(str.toLowerCase)
				} else {
					wordIndexer.addAndGetIndex(str)
				}
		}
	}
	def str2wTest(str:String,matchNum:Boolean=true):Int = {
		assert(W == wordIndexer.size + 1)
		assert(UNK == wordIndexer.size)
		val w = (str,matchNum) match {
			case (HasNum(prefix,num,suffix),true) => NUM
			case _ => 
				if(O.ignoreCase) {
					wordIndexer.indexOf(str.toLowerCase)
				} else {
					wordIndexer.indexOf(str)
				}
			}
			assert(w < wordIndexer.size, "Invalid word returned")
			if(w < 0) UNK else w
	}
	
	def pos2str(pos:Int):String = 
		if(pos < posIndexer.size) posIndexer.get(pos) else "--UNK--"
	def str2pos(str:String):Int = {
		if(frozen){ throw fail("Indexer is now immutable") }
		posIndexer.addAndGetIndex(str)
	}
	def str2posTest(str:String):Int = {
		val p:Int = posIndexer.indexOf(str)
		if(p < 0) PUNK else p
	}
	def sent2str(sent:Array[Int]) = sent.map(w2str(_)) mkString " "

	def freeze {
		frozen = true;
	}
}

/**
	Globally accessible values
*/
object G {
	val idStringMap = new HashMap[Int,String]
	val df = new DecimalFormat("0.000")
	val pf = new DecimalFormat("0.0")
	val random:scala.util.Random =
		if(O.useSeed) new scala.util.Random(O.seed) else new scala.util.Random 
	
	val CanInt = """^\-?[0-9]+(\.0+)?(E[0-9]+)?$""".r
	
	val morphology = new Morphology
}

/**
	Global utility functions
*/
object U {
	def shape(str:String):String = {
		//(compress character)
		def toChar(c:Char):Char = {
			if(c.isUpper) 'A' 
			else if(c.isDigit) '#'
			else if(c.isLower) 'a' 
			else '.' 
		}
		val chars = str.toCharArray
		if(chars.length < 4){
			//(case: short string)
			chars.map{ toChar(_) }.mkString("")
		} else {
			//(case: long string)
			val prefix = chars.slice(0,2).map{toChar(_)}.mkString("")
			val infix = chars.slice(2,chars.length-2).foldLeft(""){ case (soFar:String,c:Char) =>
				val compressed = toChar(c)
				if(soFar.length > 0 && soFar(0) == compressed){
					soFar
				} else {
					compressed + soFar
				}
			}.reverse
			val suffix = chars.slice(chars.length-2,chars.length).map{toChar(_)}.mkString("")
			prefix + infix + suffix
		}
	}
	def accum[E](fn:(E=>Unit)=>Unit,aux:E=>Unit = (e:E)=>{}):List[E] = {
		var lst:List[E] = List[E]()
		fn( (e:E) => {
			aux(e)
			lst = e :: lst
		})
		lst.reverse
	}

	def sumDiff(diff:(Duration,Duration)):Int = {
		val secA:Long = diff._1.seconds.abs
		val secB:Long = diff._2.seconds.abs
		if(secA >= Int.MaxValue-secB){
			Int.MaxValue
		} else if(secB >= Int.MaxValue-secA){
			Int.MaxValue
		} else {
			assert( (secA+secB).intValue == secA+secB, "Integer overflow error" )
			(secA+secB).intValue
		}
	}

	def safeLn(d:Double):Double = {
		assert(!d.isNaN, "Taking the log of NaN")
		assert(d >= 0, "Taking the log of a negative number")
		if(d == 0.0){ 
			Double.NegativeInfinity
		} else { 
			scala.math.log(d) 
		}
	}
	def safeLn(d:Double,fallback:Double):Double = {
		if(d.isNaN){
			safeLn(fallback)
		} else {
			safeLn(d)
		}
	}

	def rand:Double = G.random.nextDouble
	def randInt(begin:Int,end:Int):Int = G.random.nextInt(end-begin)+begin

	def sigmoid(d:Double):Double = 1.0 / (1.0 + math.exp(-d))

	private lazy val badTimexes:Set[Int] = {
		import scala.io.Source.fromFile
		import java.io.File
		if(O.badTimexes == null || O.badTimexes.equalsIgnoreCase("false")){
			Set.empty
		} else {
			fromFile(new File(O.badTimexes)).getLines
				.filter{ (str:String) =>
					!str.trim.equals("") && !str.trim.startsWith("#")}
				.map{ (str:String) => str.toInt }.toArray.toSet
		}
	}
	def timexOK(tid:Int):Boolean = {
		!badTimexes.contains(tid)
	}

	def intStore(capacity:Int):CountStore[Int] = {
		val counts:Array[Double] = new Array[Double](capacity)
		new CountStore[Int] {
			var totalCnt:Double = 0.0
			override def getCount(key:Int):Double = counts(key)
			override def setCount(key:Int,count:Double):Unit = { 
				totalCnt += count - counts(key)
				counts(key) = count 
			}
			override def emptyCopy:CountStore[Int] = intStore(capacity)
			override def clone:CountStore[Int] = {
				super.clone
				val copy = emptyCopy
				counts.zipWithIndex.foreach{ case (count:Double,i:Int) =>
					copy.setCount(i,count)
				}
				copy
			}
			override def clear:CountStore[Int] = {
				(0 until counts.length).foreach{ (i:Int) => counts(i) = 0 }
				this
			}
			override def iterator:java.util.Iterator[Int] = {
				var nextIndex = 0
				new java.util.Iterator[Int] {
					override def hasNext:Boolean = nextIndex < counts.length
					override def next:Int = {
						if(nextIndex >= counts.length){ throw new NoSuchElementException }
						nextIndex += 1
						nextIndex - 1
					}
					override def remove:Unit = {}
				}
			}
			override def totalCount:Double = totalCnt
			override def domainSize:Double = capacity
		}
	}
			
}

//------------------------------------------------------------------------------
// AUXILLIARY
//------------------------------------------------------------------------------
@SerialVersionUID(1L)
case class TimeSent(words:Array[Int],pos:Array[Int],
		nums:Array[Int],ordinality:Array[NumberType.Value],
		index:Indexing)
		extends Sentence{
	//<<error checks>>
	assert(words.zip(nums).forall{ case (w,n) => 
		(w == index.NUM && n != Int.MinValue) || (w != index.NUM && n == Int.MinValue) },
		"Words and numbers should match: "+this.toString+" :: "+nums.mkString(","))
	assert(words.length == pos.length, "word and pos lengths must match")
	assert(words.length == nums.length, "word and nums lengths must match")
	assert(words.length == ordinality.length, 
		"word and num types lengths must match")
	//<<methods>>
	def reIndex(newIndex:Indexing):TimeSent = {
		TimeSent(
			words.zipWithIndex.map{ case (w:Int,i:Int) => 
				if(w == index.NUM){
					assert(this.nums(i) != Int.MinValue, "Pushing inconsistent number!")
					newIndex.NUM
				} else {
					val cand = newIndex.str2wTest(index.w2str(w), false)   
					assert(cand != newIndex.NUM, "Created number that shouldn't be there!")
					cand
				}
			},
			pos.map{ (p:Int) => newIndex.str2posTest(index.pos2str(p)) },
			nums, ordinality, newIndex
		)
	}

	var meta:Option[(String,Int)] = None
	def tagMetaInfo(doc:String,sentI:Int):TimeSent = {
		this.meta = Some((doc,sentI))
		this
	}
	def slice(begin:Int,end:Int):TimeSent = {
		TimeSent(
			words.slice(begin,end),
			pos.slice(begin,end),
			nums.slice(begin,end),
			ordinality.slice(begin,end),
			index
		)
	}
	def shape(index:Int):String = {
		U.shape(this.index.w2str(words(index)))
	}
	def toGlossArray:Array[String] = {
		(0 until length).map{ gloss(_) }.toArray
	}
	//<<required overrides>>
	override def apply(i:Int):Int = words(i)
	override def length:Int = words.length
	override def gloss(i:Int):String = {
		if(words(i) == index.NUM){
			assert(nums(i) != Int.MinValue, 
				"Returning number that was not set: "+index.w2str(words(i))+" "+nums(i))
			nums(i).toString
		} else {
			index.w2str(words(i))
		}
	}
	//<<optional overrides>>
	override def asNumber(i:Int):Int = {
		assert(nums(i) >= 0 || words(i) != index.NUM, 
			"Num has no numeric value: " + gloss(i))
		nums(i)
	}
	override def asDouble(i:Int):Double = nums(i).toDouble
	//<<object overrides>>
	override def toString:String 
		= words
			.zipWithIndex
			.map{ case (w:Int,i:Int) =>
				if(w == index.NUM) nums(i) else index.w2str(w) 
			}.mkString(" ")
}



//------------------------------------------------------------------------------
// DATA
//------------------------------------------------------------------------------
case class TimeData[T](train:DataStore[T],eval:DataStore[T]) {
	def noopLoop(fn:T=>Any):Unit = {
		train.eachExample( -1 ).foreach{ fn(_)  }
		eval.eachExample( -1 ).foreach{ fn(_)  }
	}
	def noopLoop:Unit = noopLoop( (t:T) => {} )
}
trait DataStore[T] {
	def name:String
	def eachExample(i:Int):Iterable[T]
}


//------------------------------------------------------------------------------
// OTHER ENTRIES
//------------------------------------------------------------------------------
object Interpret {
	DateTimeZone.setDefault(DateTimeZone.UTC);
	def main(args:Array[String]) {
		System.setProperty("gutime","aux/gutime")
		//--Redwood
		val props = new Properties();
		props.setProperty("log.neatExit", "true");
		props.setProperty("log.collapse", "approximate");
		props.setProperty("log.toStderr", "false");
		StanfordRedwoodConfiguration.apply(props);
		//--Variables
		if(args.length != 4){
			println("USAGE: Interpret tempeval model data tempeval_home")
		}
		val typ = args(0)
		if(typ.toLowerCase == "tempeval"){
			//--Case: Tempeval
			//(args)
			val modelStr = args(1)
			val dataStr = args(2)
			val tempevalHome = args(3)
			//(model)
			val model:Annotator = {
				if(new File(modelStr).exists){
					log(FORCE,"Reading model from " + modelStr)
					edu.stanford.nlp.io.IOUtils.readObjectFromFile(modelStr)
				} else {
					log(FORCE,"Creating new " + modelStr)
					new MetaClass(modelStr).createInstance()
				}
			}
			//(data)
			val (trainData,testData) = /*model match*/ {
//				case (tt:TreeTime) =>
//					val rawData = new TimeDataset(new SerializedCoreMapDataset(dataStr))
//		  		val data = TimeData(
//						new GroundingData( rawData.train, true, tt.index),
//						new GroundingData( rawData.test, false, tt.index))
//					val trainData = data.train.asInstanceOf[Iterable[Annotation]]
//					val testData = data.eval.asInstanceOf[Iterable[Annotation]]
//					(trainData,testData)
//				case _ =>
					val rawData = new TimeDataset(new SerializedCoreMapDataset(dataStr))
					val trainData = rawData.train.data.map{ (datum:CoreMapDatum) =>
							datum.impl.asInstanceOf[Annotation]
						}
					val testData = rawData.test.data.map{ (datum:CoreMapDatum) =>
							datum.impl.asInstanceOf[Annotation]
						}
					(trainData,testData)
			}
			//(script)
			val (trainOfficial,trainAngel)
				= Entry.officialEval(model,trainData,true,new File(tempevalHome))
			log(GREEN,FORCE,"TRAIN official: " + trainOfficial)
			log(GREEN,FORCE,"TRAIN angel:    " + trainAngel)
//			val (testOfficial,testAngel)
//				= Entry.officialEval(model,testData,false,new File(tempevalHome))
//			log(GREEN,FORCE,"TEST official: " + testOfficial)
//			log(GREEN,FORCE,"TEST angel:    " + testAngel)
		}
	}
}

//------------------------------------------------------------------------------
// ENTRY
//------------------------------------------------------------------------------
case class OfficialScore(
		detectPrecision:Double,
		detectRecall:Double,
		typeAccuracy:Double,
		valueAccuracy:Double){
	def detectF1:Double = {
		2.0*detectPrecision*detectRecall / (detectPrecision + detectRecall)
	}
	override def toString:String =
		"detect=[P:"+G.df.format(detectPrecision)+" R:"+G.df.format(detectRecall)+" F1:"+
			G.df.format(detectF1)+"] interprt=[typ:" + G.df.format(typeAccuracy) + 
			" val:"+G.df.format(valueAccuracy)+"]"
}

case class AngelScore(
		typePrecision:Double,
		typeRecall:Double,
		valuePrecision:Double,
		valueRecall:Double){
	
	def typeF1:Double = 2.0*typePrecision*typeRecall / (typePrecision + typeRecall)
	def valueF1:Double = 2.0*valuePrecision*valueRecall / (valuePrecision + valueRecall)
	
	override def toString:String =
		"type=[P:"+G.df.format(typePrecision)+" R:"+G.df.format(typeRecall)+" F1:"+
			G.df.format(typeF1)+"] value=[P:" + G.df.format(valuePrecision) + 
			" R:"+G.df.format(valueRecall)+" F1:" + G.df.format(valueF1) + "]"
}


class Entry {}
object Entry {
	import edu.stanford.nlp.util.logging._
	
	def angelScript(
			train:Boolean,
			tempevalHome:File,
			guessExtentsRaw:Option[File],
			guessAttributesRaw:Option[File]
			):AngelScore = {
		//--Setup
		//(get other paths)
		val base:String =
			if(!train){
				tempevalHome.getPath+"/test/english/key"
			} else {
				tempevalHome.getPath+"/training/english/data"
			}
		val scorer:String = tempevalHome.getPath+"/scorer/score_entities_extended.py"
		val segments:String = base + "/base-segmentation.tab"
		val goldExtents:String = base + "/timex-extents.tab"
		val goldAttributes:String = base + "/timex-attributes.tab"
		//(get input paths)
		val guessExtents = guessExtentsRaw.getOrElse(new File(goldExtents)).getPath
		val guessAttributes = guessAttributesRaw.getOrElse(new File(goldAttributes)).getPath
		//--Run
		//(run script)
		val scriptOutput:String = {
			val cmd = 
				(new StringBuilder).append("python").append(" ")
					.append(scorer).append(" ")
					.append(segments).append(" ")
					.append(goldExtents).append(" ")
					.append(guessExtents).append(" ")
					.append(goldAttributes).append(" ")
					.append(guessAttributes).append(" ")
					.toString
			try {
				cmd !!;
			} catch {
				case (e:RuntimeException) => {
					err("SCRIPT FAILED: " + e.getMessage);
					err(cmd);
					"""
					NO GUESSES
					precision   1.0
					recall      0.0
					attribute type       1.0 | 1.0 0.0 0.0
					attribute value      1.0 | 1.0 0.0 0.0
					"""
				}
				case _ => throw new IllegalStateException("???")
			}
		}
		//--Create Score
		//(print output)
		startTrack("Script Output")
		log(scriptOutput)
		endTrack("Script Output")
		//(define scores)
		val Prec      = """(?ms).*precision   ([0-9\.]+).*""".r
		val Recall    = """(?ms).*recall      ([0-9\.]+).*""".r
		val Type  =  """(?ms).*attribute type       ([0-9\.]+) \| ([0-9\.]+) ([0-9\.]+) ([0-9\.]+).*""".r
		val Value =  """(?ms).*attribute value      ([0-9\.]+) \| ([0-9\.]+) ([0-9\.]+) ([0-9\.]+).*""".r
		val MType  =  """(?ms).*mention attribute type       ([0-9\.]+) \| ([0-9\.]+) ([0-9\.]+) ([0-9\.]+).*""".r
		val MValue =  """(?ms).*mention attribute value      ([0-9\.]+) \| ([0-9\.]+) ([0-9\.]+) ([0-9\.]+).*""".r
		//(digest scores)
		val MType(typeOrig,typePrec,typeRec,typF1) = scriptOutput
		val MValue(valueOrig,valuePrec,valueRec,valueF1) = scriptOutput
		//(return score
		AngelScore(typePrec.toDouble, typeRec.toDouble, 
			valueRec.toDouble,valuePrec.toDouble)
	}

	def officialScript(
			train:Boolean,
			tempevalHome:File,
			guessExtentsRaw:Option[File],
			guessAttributesRaw:Option[File]
			):OfficialScore = {
		//--Setup
		//(get other paths)
		val base:String =
			if(!train){
				tempevalHome.getPath+"/test/english/key"
			} else {
				tempevalHome.getPath+"/training/english/data"
			}
		val scorer:String = tempevalHome.getPath+"/scorer/score_entities.py"
		val segments:String = base + "/base-segmentation.tab"
		val goldExtents:String = base + "/timex-extents.tab"
		val goldAttributes:String = base + "/timex-attributes.tab"
		//(get input paths)
		val guessExtents = guessExtentsRaw.getOrElse(new File(goldExtents)).getPath
		val guessAttributes = guessAttributesRaw.getOrElse(new File(goldAttributes)).getPath
		//--Run
		//(run script)
		val scriptOutput:String = {
			val cmd = 
				(new StringBuilder).append("python").append(" ")
					.append(scorer).append(" ")
					.append(segments).append(" ")
					.append(goldExtents).append(" ")
					.append(guessExtents).append(" ")
					.append(goldAttributes).append(" ")
					.append(guessAttributes).append(" ")
					.toString
			try {
				cmd !!;
			} catch {
				case (e:RuntimeException) => {
					err("SCRIPT FAILED: " + e.getMessage);
					err(cmd);
					"""
					NO GUESSES
					precision   1.0
					recall      0.0
					attribute type       1.0
					attribute value      1.0
					"""
				}
				case _ => throw new IllegalStateException("???")
			}
		}
		//--Create Score
		//(print output)
		startTrack("Script Output")
		log(scriptOutput)
		endTrack("Script Output")
		//(get scores)
		val Prec   = """(?ms).*precision   ([0-9\.]+).*""".r
		val Recall = """(?ms).*recall      ([0-9\.]+).*""".r
		val Type =  """(?ms).*attribute type       ([0-9\.]+).*""".r
		val Value = """(?ms).*attribute value      ([0-9\.]+).*""".r
		val Prec(detectPrec) = scriptOutput
		val Recall(detectRec) = scriptOutput
		val Type(typAccr) = scriptOutput
		val Value(valAccr) = scriptOutput
		//(return score
		OfficialScore(detectPrec.toDouble, detectRec.toDouble, 
			typAccr.toDouble,valAccr.toDouble)
	}
			
	private case class TimexSpan(timex:StanfordTimex,begin:Int,sentI:Int){ 
		var end:Int = begin+1 
	}

	private def annotateSpans(sys:Annotator,doc:Annotation):Buffer[TimexSpan] = {
			sys.annotate(doc)
			//--Collect Timexes
			//(utility)
			def isTimex(lbl:CoreLabel):Boolean 
				= lbl.get[StanfordTimex,TimexAnnotation](classOf[TimexAnnotation]) != null
			//(for each sentence)
			doc.get[JList[CoreMap],SentencesAnnotation](classOf[SentencesAnnotation])
					.zipWithIndex.flatMap{ case (sent:CoreMap,sentI:Int) =>
				if(sent.get[JList[CoreMap],TimexAnnotations](classOf[TimexAnnotations])
						!= null){
					//--Case: Central Timexes
					val timexes:Buffer[CoreMap] = sent.get[JList[CoreMap],TimexAnnotations](
							classOf[TimexAnnotations])
					timexes.map{ (expr:CoreMap) =>
						val timex = expr.get[StanfordTimex,TimexAnnotation](classOf[TimexAnnotation])
						val begin = expr.get[JInt,TokenBeginAnnotation](classOf[TokenBeginAnnotation])
						val end = expr.get[JInt,TokenEndAnnotation](classOf[TokenEndAnnotation])
						val span = TimexSpan(timex,begin,sentI)
						span.end = end
						span
					} 
				} else {
					//--Case: No Central Timexes
					//((get tokens))
					val tokens:Buffer[CoreLabel] 
						= sent.get[JList[CoreLabel],TokensAnnotation](
						classOf[TokensAnnotation])
					//((cycle over tokens))
					val (revTimexes,endedOnTimex)
						= tokens.zipWithIndex.foldLeft(List[TimexSpan](),false){ 
							case ((timexes:List[TimexSpan],lastTimex:Boolean),
							      (tok:CoreLabel,index:Int)) =>
						if(!lastTimex && isTimex(tok)){
							//(case: start timex)
							val timex = tok.get[StanfordTimex,TimexAnnotation](classOf[TimexAnnotation])
							var begin = tok.get[JInt,TokenBeginAnnotation](classOf[TokenBeginAnnotation])
							if(begin == null){ begin = index }
							(TimexSpan(timex,begin,sentI) :: timexes, true)
						} else if(lastTimex && isTimex(tok)){
							//(case: in timex)
							(timexes, true)
						} else if(!lastTimex && !isTimex(tok)){
							//(case: not in timex)
							(timexes, false)
						} else if(lastTimex && !isTimex(tok)){
							//(case: ended timex)
							var end = tokens.get(index-1).get[JInt,TokenEndAnnotation](classOf[TokenEndAnnotation])
							if(end == null){ end = index }
							timexes.head.end = end
							(timexes, false)
						} else {
							throw new IllegalStateException("impossible")
						}
					}
					//(last timex)
					if(endedOnTimex){
						var end = tokens.get(tokens.length-1).get[JInt,TokenEndAnnotation](classOf[TokenEndAnnotation])
						if(end == null){ end = tokens.length }
						revTimexes.head.end = end
					}
					//--Append
					revTimexes.reverse
				}
			}
		
	}


	def officialEval(sys:Annotator,data:Iterable[Annotation],train:Boolean,
			tempevalHome:File, 
			attributeFile:File=File.createTempFile("attr",".tab"),
			extentsFile:File=File.createTempFile("ext",".tab")
			):(OfficialScore,AngelScore) = {
		startTrack("Evaluating " + sys.getClass + " on " + {if(train) "train" else "test" })
		//--Setup
		if(!attributeFile.exists){ attributeFile.createNewFile }
		val attrs = new StringBuilder
		val extents = new StringBuilder
		//--Annotate
		data.foreach{ (doc:Annotation) =>
			//--Variables
			val docID = doc.get[String,DocIDAnnotation](classOf[DocIDAnnotation])
			var tidNum = 0
			val ground = new DateTime(
					doc.get[Calendar,CalendarAnnotation](classOf[CalendarAnnotation])
					.getTimeInMillis )
			startTrack("Document " + docID)
			//--Process Spans
			annotateSpans(sys,doc).foreach{ (span:TimexSpan) =>
				//(variables)
				tidNum += 1
				def base(b:StringBuilder,tok:Int) = b.append("\n")
					.append(docID).append("\t")
					.append(span.sentI).append("\t")
					.append(tok).append("\t")
					.append("timex3").append("\t")
					.append("t").append(tidNum).append("\t")
					.append("1").append("\t")
				//(value)
				if(span.timex.timexType != null){
					base(attrs,span.begin)
						.append("type").append("\t")
						.append(span.timex.timexType)
				}
				if(span.timex.value != null){
					base(attrs,span.begin)
						.append("value").append("\t")
						.append(DataLib.patchAttribute(
							span.timex.timexType,
							span.timex.value,
							ground))
				}
				//(extent)
				(span.begin until span.end).foreach{ (tokI:Int) =>
					base(extents,tokI)
				}
			}
			endTrack("Document " + docID)
		}
		//--Write
		//(attributes)
		val fw = new FileWriter(attributeFile)
		fw.write(if(attrs.length == 0) "" else attrs.substring(1))
		fw.close()
		//(extents)
		val fw2 = new FileWriter(extentsFile)
		fw2.write(if(extents.length == 0) "" else extents.substring(1))
		fw2.close()
		//--Score
		val official = officialScript(train,tempevalHome,Some(extentsFile),Some(attributeFile))
		val angel = angelScript(train,tempevalHome,Some(extentsFile),Some(attributeFile))
		endTrack("Evaluating " + sys.getClass + " on " + {if(train) "train" else "test" })
		(official,angel)
	}



	def main(args:Array[String]):Unit = {
		//--Exec
		Execution.exec(new Runnable(){
			override def run:Unit = {
				O.mode match {
					//(case: time expression console)
					case O.RunMode.Console => Temporal.interactive
					//(case: interpret)
					case O.RunMode.Interpret => (new InterpretationTask).run
					case O.RunMode.Detect => (new DetectionTask).run
					case O.RunMode.System => throw fail("NOT IMPLEMENTED")
				}
			}
		}, args, new StanfordExecutionLogInterface)
	}
}
