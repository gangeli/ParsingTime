package time

import java.util.{List => JList};
import java.util.Calendar;
import java.util.Properties;
import java.io.File;
import java.io.FileWriter;
import java.io.Serializable;

import scala.collection.JavaConversions._
import scala.collection.mutable.Map
import scala.sys.process._

import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.util._
import edu.stanford.nlp.time.{Timex => StanfordTimex}
import edu.stanford.nlp.time.GUTimeAnnotator
import edu.stanford.nlp.time.HeidelTimeAnnotator
import edu.stanford.nlp.time.TimeAnnotator
import edu.stanford.nlp.time.TimeAnnotations._
import edu.stanford.nlp.time.JodaTimeUtils
import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.util.logging.PrettyLogger
import edu.stanford.nlp.util.logging.StanfordRedwoodConfiguration
import edu.stanford.nlp.io.IOUtils

import org.joda.time._

import org.goobs.stanford.SerializedCoreMapDataset
import org.goobs.stanford.CoreMapDatum

//------------------------------------------------------------------------------
// FRAMEWORK
//------------------------------------------------------------------------------
case class SystemOutput(typ:Option[String],value:Option[String])
	extends Serializable
case class SystemInput(gloss:String,typ:String,value:String,
	ground:DateTime,isTest:Boolean,timex:Timex,
	output:(Option[String],Option[String])=>String)
case class AttributeScore(
		typeAccuracy:Double,valueAccuracy:Double,raw:String,
		exactValueMatch:Double,percentAttempted:Double){
	def accuracy:Double = valueAccuracy
	override def toString:String = {
		"type: " + G.df.format(typeAccuracy) + 
			" value: " + G.df.format(valueAccuracy) + 
			" exact: " + G.df.format(exactValueMatch)
	}
}

trait OtherSystem {
	def getTimex(input:SystemInput,minProb:Double):Option[SystemOutput]
	def name:String

	def prCurve(data:Iterable[SystemInput],isTest:Boolean,
			quiet:Boolean,files:File*):Unit = {
		var b = new StringBuilder
		b.append("# MinProb\tPercentAttempted\tTypeAccuracy\tValueAccuracy\tValueExactMatch\n")
		//--Get Scores
		for( prob <- 1.0 to 0.0 by -0.001 ){
			startTrack("Prob Bound " + prob)
			val score:AttributeScore = evaluateAttributes(data,isTest,true,prob,false,
				File.createTempFile("attr", ".tab"))
			b.append(prob)
				.append("\t").append(G.df.format(score.percentAttempted))
				.append("\t").append(G.df.format(score.typeAccuracy))
				.append("\t").append(G.df.format(score.valueAccuracy))
				.append("\t").append(G.df.format(score.exactValueMatch))
				.append("\n")
			endTrack("Prob Bound " + prob)
		}
		//--Write File
		files.foreach{ (f:File) =>
			try {
				if(!f.exists){ f.createNewFile }
				val fw = new FileWriter(f)
				fw.write(b.toString)
				fw.close()
			} catch {
				case (e:Exception) => throw new RuntimeException(e)
			}
		}
	}

	def evaluateAttributes(data:Iterable[SystemInput],isTest:Boolean,
			quiet:Boolean,probLowerBound:Double,force:Boolean,
			files:File*):AttributeScore = {
		//--Run
		startTrack("Evaluating")
		var exactMatch:Int = 0
		var attempted:Int = 0
		var totalCount:Int = 0
		var timexesNotAttempted:List[Timex] = List[Timex]()
		val guessRaw = data.zipWithIndex.foldLeft((new StringBuilder)){ 
				case (b:StringBuilder,(input:SystemInput,i:Int)) =>
			//(score)
			val guess:Option[SystemOutput] = getTimex(input,probLowerBound)
			//(calculate exact match
			guess match {
				case Some(output) => output.value match {
					case Some(timexValue) =>
						if(input.value.equals(timexValue)){ exactMatch += 1 }
					case None =>
				}
				case None =>
			}
			totalCount += 1
			//(log)
			startTrack("Timex " + i + ": " + input.gloss)
			log("guess:  " + guess)
			if(!quiet || !isTest){ log("gold:  " + input.value) }
			log("ground: " + input.ground)
			endTrack("Timex " + i + ": " + input.gloss)
			//(create string)
			guess match {
				case Some(guessVal) =>
					//(case: attempted)
					attempted += 1
					b.append("\n")
					b.append(input.output(guessVal.typ,guessVal.value))
				case None => 
					//(case: not attempted)
					if(force){ 
						//(force a failure)
						b.append("\n")
						b.append(input.output(None,None)) 
					} else { 
						//(accept not attempted)
						timexesNotAttempted = input.timex :: timexesNotAttempted
						b 
					}
			}
		}
		val guess = guessRaw.substring(if(guessRaw.length > 0) 1 else 0).toString
		endTrack("Evaluating")
		//--Save Temp
		startTrack("Setting Up Script")
		//(save to file)
		def write(f:File):Unit = {
			try {
				if(!f.exists){ f.createNewFile }
				val fw = new FileWriter(f)
				fw.write(guess)
				fw.close()
			} catch {
				case (e:Exception) => throw new RuntimeException(e)
			}
		}
		//(save other files)
		files.foreach{ write(_) }
		//(get other paths)
		val base:String =
			if(isTest){
				Comparisons.inputDir.getPath+"/test/"+
					Comparisons.lang+"/key/"
			} else {
				Comparisons.inputDir.getPath+"/training/"+
					Comparisons.lang+"/data/"
			}
		val scorer:String = Comparisons.inputDir.getPath+"/scorer/score_entities.py"
		val segments:String = base + "base-segmentation.tab"
		val goldExtents:String = base + "timex-extents.tab"
		val goldAttributes:String = base + "timex-attributes.tab"
		//(get extents)
		val guessExtents:String = File.createTempFile("guessExtent",".tab").getPath
//		val guessExtents:String = new File("asdf").getPath; (new File("asdf")).createNewFile
		val allExtentLinesJ:JList[String] = IOUtils.linesFromFile(goldExtents)
		val allExtentLines:List[String] = allExtentLinesJ.map{ x => x }.toList
		val keptExtentLines = timexesNotAttempted.foldLeft(allExtentLines){
				case (okLines:List[String], timex:Timex) =>
			timex.filterFromExtent(okLines)
		}
		try {
			val fw = new FileWriter(new File(guessExtents))
			fw.write(keptExtentLines mkString "\n")
			fw.close()
		} catch {
			case (e:Exception) => throw new RuntimeException(e)
		}
		//(log)
		log(FORCE,"Scorer:           " + scorer)
		log(FORCE,"Segments:         " + segments)
		log(FORCE,"Gold extents:     " + goldExtents)
		log(FORCE,"Guess extents:    " + guessExtents)
		log(FORCE,"Gold attributes:  " + goldAttributes)
		log(FORCE,"Guess attributes: " + files(0))
		endTrack("Setting Up Script")
		//--Score
		startTrack("Scoring")
		import Process._
		import org.goobs.slib.Static._
		//(run script)
		val scriptOutput:String = {
			try {
				(new StringBuilder).append("python").append(" ")
					.append(scorer).append(" ")
					.append(segments).append(" ")
					.append(goldExtents).append(" ")
					.append(guessExtents).append(" ")
					.append(goldAttributes).append(" ")
					.append(files(0)).append(" ")
					.toString !!;
			} catch {
				case (e:RuntimeException) => {
					err("SCRIPT FAILED: " + e.getMessage);
					"""
					NO GUESSES
					attribute type       1.0
					attribute value      1.0
					"""
				}
				case _ => throw new IllegalStateException("???")
			}
		}
		//(print output)
		startTrack("Script Output")
		if(!quiet || !isTest){ log(scriptOutput) }
		endTrack("Script Output")
		//(get scores)
		val Type =  """(?ms).*attribute type       ([0-9\.]+).*""".r
		val Value = """(?ms).*attribute value      ([0-9\.]+).*""".r
		val Type(typAccr) = scriptOutput
		val Value(valAccr) = scriptOutput
		//(return)
		if(!quiet || !isTest){
			log("type:  " + typAccr)
			log("value: " + valAccr)
		}
		endTrack("Scoring")
		AttributeScore(typAccr.toDouble,valAccr.toDouble,scriptOutput,
			exactMatch.asInstanceOf[Double] / totalCount.asInstanceOf[Double],
			attempted.asInstanceOf[Double] / totalCount.asInstanceOf[Double])
	}

}

//------------------------------------------------------------------------------
// IMPLEMENTATIONS
//------------------------------------------------------------------------------
object CoreNLPSystem {
	val pipeline:AnnotationPipeline = {
			val props = new Properties
			props.setProperty("pos.model",
				System.getenv("HOME") +
					"/lib/data/bidirectional-distsim-wsj-0-18.tagger")
			props.setProperty("annotators","tokenize, ssplit, pos")
			val pipe = new StanfordCoreNLP(props);
			pipe
		}
	
}

class CoreNLPSystem(nm:String,annotator:Annotator) extends OtherSystem {
	override def name:String = nm
	override def getTimex(input:SystemInput,minProb:Double):Option[SystemOutput]={
		//--Create CoreMap
		//(create)
		val doc:Annotation = new Annotation(input.gloss)
		//(set calendar)
		doc.set(classOf[CalendarAnnotation], 
			input.ground.toGregorianCalendar.asInstanceOf[Calendar])
		doc.set(classOf[DocDateAnnotation], 
			String.format("%TF",
				input.ground.toGregorianCalendar.asInstanceOf[Calendar]))
		//(annotate)
		CoreNLPSystem.pipeline.annotate(doc)
		//(time annotation)
		annotator.annotate(doc)
		//--Process Timexes
		//(extract)
		var timex:Option[StanfordTimex] = None
		doc.get[
				JList[CoreMap],TimexAnnotations](
				classOf[TimexAnnotations]).foreach{ case (map:CoreMap) =>
				val timexCand:StanfordTimex = map.get[StanfordTimex,TimexAnnotation](
					classOf[TimexAnnotation])
				timex = if(timexCand != null){ 
						timex match {
							case Some(st) =>
								if(st.value == null){
									if(timexCand.value == null){ timex }  //case: both timexes bad
									else{ Some(timexCand) }               //case: new better
								} else {
									timex                                 //case: old complete
								}
							case None => Some(timexCand)              //case: no old
						}
					} else {
						timex                                       //case: no new
					}
		}
		//(process)
		timex match {
			case Some(t) => Some(SystemOutput(
				{if(t.timexType == null) None else Some(t.timexType)},
				{if(t.value == null) None else Some(t.value)}))
			case None => None
		}
	}
}


case class MySystemInfo(valueMap:Map[Int,(Option[SystemOutput],Int,Double)])
		extends OtherSystem with Serializable {
	override def name:String = "MySystem"
	override def getTimex(input:SystemInput,minProb:Double):Option[SystemOutput]={
		//(get output)
		val (output,len,lprob) = valueMap(input.timex.index)
		//(get average combination probability)
		val numRules:Int = 2*len-1
		val aveRuleProb:Double = math.exp(lprob*(1.0/numRules.asInstanceOf[Double]))
		//(return)
		if(aveRuleProb >= minProb){
			output.map{ x => x }
		} else { 
			None
		}
	}
}

//------------------------------------------------------------------------------
// MAIN
//------------------------------------------------------------------------------
object Comparisons {
	var inputDir:File = null
	var outputDir:File = null
	var lang:String = null

	val GUTime:CoreNLPSystem = 
		new CoreNLPSystem("GUTime",new GUTimeAnnotator(new File(
			System.getenv("HOME")+"/workspace/time/etc/")))

	val SUTime:CoreNLPSystem = new CoreNLPSystem("SUTime",new TimeAnnotator)
	
	val HeidelTime:CoreNLPSystem = new CoreNLPSystem("HeidelTime",
		new HeidelTimeAnnotator(
			new File(System.getenv("HOME")+"/workspace/time/etc/")))

	def outputFile(sys:OtherSystem,isTest:Boolean) = new File(
			Comparisons.outputDir.getPath+
				"/attributes-"+sys.name+"-"+
				{if(isTest) "test" else "train"}+".tab"
		)
	def prFile(dir:String,isTest:Boolean)
			= new File(dir + "/pr-"+{if(isTest) "test" else "train"}+".dat")
	
	def dataset2inputs(raw:TimeDataset):Array[SystemInput] = {
		val dataLst
			= raw.timexes.foldLeft((List[SystemInput]())){
				case ((data:List[SystemInput]),timex:Timex) =>
			SystemInput(	
				timex.gloss,
				timex.originalType,
				timex.originalValue,
				timex.grounding.base,
				timex.isTest,
				timex,
				timex.tempevalAttribute(_,_,timex.grounding.base,true)) :: data
		}
		dataLst.reverse.toArray
	}

	def runSystem(sys:OtherSystem,quiet:Boolean=false
			):(AttributeScore,AttributeScore) = {
		//--Score Dataset
		def score(data:TimeDataset,isTest:Boolean) = {
			sys.evaluateAttributes(
				dataset2inputs(data),isTest,quiet,0.0,true,outputFile(sys,isTest))
		}
		//--Run
		//(get data)
		val data = new TimeDataset(new SerializedCoreMapDataset(
			System.getenv("HOME") + 
				"/workspace/time/aux/coremap/tempeval2-english-retok-numbers"
			))
		//(score)
		startTrack("Train")
		val train = score(data.train,false)
		endTrack("Train")
		startTrack("Test")
		val test = score(data.test,true)
		endTrack("Test")
		//(return)
		( train, test )
	}

	def prCurve(sys:OtherSystem,dir:String,quiet:Boolean=true):Unit = {
		//--Score Dataset
		def score(data:TimeDataset,isTest:Boolean) = {
			sys.prCurve(
				dataset2inputs(data),isTest,quiet,prFile(dir,isTest))
		}
		//--Run
		//(get data)
		val data = new TimeDataset(new SerializedCoreMapDataset(
			System.getenv("HOME") + 
				"/workspace/time/aux/coremap/tempeval2-english-retok-numbers"
			))
		//(score)
		startTrack("Train")
		val train = score(data.train,false)
		endTrack("Train")
		startTrack("Test")
		val test = score(data.test,true)
		endTrack("Test")
	}

	def main(args:Array[String]) = {
		//--Setup
		//(jodatime)
		DateTimeZone.setDefault(DateTimeZone.UTC);
		//(arguments)
		outputDir = new File(args(0))
		assert(outputDir.exists, "No such file: " + outputDir)
		assert(outputDir.isDirectory, "Not a directory: " + outputDir)
		inputDir = new File(args(1))
		assert(inputDir.exists, "No such file: " + inputDir)
		assert(inputDir.isDirectory, "Not a directory: " + inputDir)
		lang = args(2)
		val typ = args(3)
		//--Routing
		//(universal)
		val RunNumber = """^([0-9]+)$""".r
		val props = new Properties;
		props.setProperty("log.collapse", "approximate");
		props.setProperty("log.neatExit", "true");
		props.setProperty("log.console.trackStyle", "BOLD");
		props.setProperty("log.toStderr", "false");
		props.setProperty("log.file", outputDir.getPath+"/log");
		StanfordRedwoodConfiguration.apply(props);
		//(switch)
		typ.toLowerCase match {
			case "other" =>
				//--Case: Other Systems
				//(logging)
				//--Systems
				startTrack("GUTime")
				val (guTrain,guTest) = runSystem(GUTime)
				endTrack("GUTime")
				startTrack("SUTime")
				val (suTrain, suTest) = runSystem(SUTime)
				endTrack("SUTime")
				startTrack("HeidelTime")
				val (heTrain,heTest) = runSystem(HeidelTime)
				endTrack("HeidelTime")
				//--Print
				startTrack("Result Summary")
				log(FORCE,"GUTime Train:     " + guTrain)
				log(FORCE,"GUTime Test:      " + guTest)
				log(FORCE,"SUTime Train:     " + suTrain)
				log(FORCE,"SUTime Test:      " + suTest)
				log(FORCE,"HeidelTime Train: " + heTrain)
				log(FORCE,"HeidelTime Test:  " + heTest)
				endTrack("Result Summary")
			case RunNumber(run) =>
				//--Case: Existing Run
				val info:MySystemInfo = IOUtils.readObjectFromFileNoExceptions(
					new File("out/"+run+".exec/tempeval2-output.ser"))
				prCurve(info, "out/"+run+".exec/")
			case _ => throw new IllegalArgumentException("Unknown case: " + typ)
		}
	}
}











