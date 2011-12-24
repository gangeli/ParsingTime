package time

import java.util.{List => JList};
import java.util.Calendar;
import java.util.Properties;
import java.io.File;
import java.io.FileWriter;

import scala.collection.JavaConversions._
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

import org.joda.time._

import org.goobs.stanford.SerializedCoreMapDataset
import org.goobs.stanford.CoreMapDatum

//------------------------------------------------------------------------------
// FRAMEWORK
//------------------------------------------------------------------------------
case class SystemOutput(typ:Option[String],value:Option[String])
case class SystemInput(gloss:String,typ:String,value:String,
	ground:DateTime,isTest:Boolean,timex:Timex,
	output:(Option[String],Option[String])=>String)
case class AttributeScore(
		typeAccuracy:Double,valueAccuracy:Double,raw:String,
		exactValueMatch:Double){
	def accuracy:Double = valueAccuracy
	override def toString:String = {
		"type: " + G.df.format(typeAccuracy) + 
			" value: " + G.df.format(valueAccuracy) + 
			" exact: " + G.df.format(exactValueMatch)
	}
}

trait OtherSystem {
	def getTimex(input:SystemInput):Option[SystemOutput]
	def name:String

	def outputFile(isTest:Boolean) = Comparisons.outputDir.getPath+
			"/attributes-"+name+"-"+{if(isTest) "test" else "train"}+".tab"

	def evaluateAttributes(data:Iterable[SystemInput],isTest:Boolean,
			quiet:Boolean,otherFiles:File*):AttributeScore = {
		//--Run
		startTrack("Evaluating")
		var exactMatch:Int = 0
		var totalCount:Int = 0
		val guess = data.zipWithIndex.foldLeft((new StringBuilder)){ 
				case (b:StringBuilder,(input:SystemInput,i:Int)) =>
			//(score)
			val guess:Option[SystemOutput] = getTimex(input)
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
					b.append("\n")
			guess match {
				case Some(guessVal) =>
					b.append(input.output(guessVal.typ,guessVal.value))
				case None => 
					b.append(input.output(None,None))
			}
		}.substring(1).toString
		endTrack("Evaluating")
		//--Save Temp
		startTrack("Setting Up Script")
		//(create file)
		val guessAttributes:String = outputFile(isTest)
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
		write(new File(guessAttributes))
		//(save other files)
		otherFiles.foreach{ write(_) }
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
		val guessExtents:String = base + "timex-extents.tab"
		val goldAttributes:String = base + "timex-attributes.tab"
		//(log)
		log(FORCE,"Scorer:           " + scorer)
		log(FORCE,"Segments:         " + segments)
		log(FORCE,"Gold extents:     " + goldExtents)
		log(FORCE,"Guess extents:    " + guessExtents)
		log(FORCE,"Gold attributes:  " + goldAttributes)
		log(FORCE,"Guess attributes: " + guessAttributes)
		endTrack("Setting Up Script")
		//--Score
		startTrack("Scoring")
		import Process._
		import org.goobs.slib.Static._
		//(run script)
		val scriptOutput:String = 
			(new StringBuilder).append("python").append(" ")
				.append(scorer).append(" ")
				.append(segments).append(" ")
				.append(goldExtents).append(" ")
				.append(guessExtents).append(" ")
				.append(goldAttributes).append(" ")
				.append(guessAttributes).append(" ")
				.toString !!;
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
			exactMatch.asInstanceOf[Double] / totalCount.asInstanceOf[Double])
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
	override def getTimex(input:SystemInput):Option[SystemOutput] = {
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

	def runSystem(sys:OtherSystem,quiet:Boolean=false
			):(AttributeScore,AttributeScore) = {
		//--Score Dataset
		def score(data:TimeDataset,isTest:Boolean) = {
			val dataLst
				= data.timexes.foldLeft((List[SystemInput]())){
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
			sys.evaluateAttributes(dataLst.reverse,isTest,quiet)
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
		//(logging)
		val props = new Properties;
		props.setProperty("log.collapse", "approximate");
		props.setProperty("log.neatExit", "true");
		props.setProperty("log.console.trackStyle", "BOLD");
		props.setProperty("log.toStderr", "false");
		props.setProperty("log.file", outputDir.getPath+"/log");
		StanfordRedwoodConfiguration.apply(props);
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
	}
}











