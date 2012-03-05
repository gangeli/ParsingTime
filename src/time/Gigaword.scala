package time

import java.util.Properties;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.{List => JList};
import java.util.ArrayList;
import java.io._;

import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import scala.io.Source.fromFile

import org.joda.time.DateTime
import org.joda.time.DateTimeZone

import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.ie.NumberNormalizer
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.util._
import edu.stanford.nlp.time.{Timex => StanfordTimex}
import edu.stanford.nlp.time.GUTimeAnnotator
import edu.stanford.nlp.tagger.maxent.MaxentTagger
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.process.WhitespaceTokenizer
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.time.TimeAnnotations._
import edu.stanford.nlp.time.JodaTimeUtils
import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.util.logging.StanfordRedwoodConfiguration;

import org.goobs.database._
import org.goobs.testing.Dataset
import org.goobs.testing.Datum
import org.goobs.stanford.JavaNLP._
import org.goobs.stanford.CoreMapDatum
import org.goobs.stanford.SerializedCoreMapDataset

//------------------------------------------------------------------------------
// SUTime Data Processor
//------------------------------------------------------------------------------
object Gigaword {
	val date = """.*id\="[^0-9]+([0-9]{4})([0-9]{2})([0-9]{2})\.[0-9]+".*""".r
	val name = """.*id\="([^"]+)".*""".r

	val pipeline:AnnotationPipeline = {
			val props = new Properties
			props.setProperty("pos.model",
				System.getenv("HOME") +
					"/lib/data/bidirectional-distsim-wsj-0-18.tagger")
			props.setProperty("annotators","tokenize, ssplit, pos")
			val pipe = new StanfordCoreNLP(props);
			pipe
		}

	val tagger = new MaxentTagger(
		System.getenv("HOME") + "/lib/data/bidirectional-distsim-wsj-0-18.tagger")

	def appendDoc(outDir:File,
			f:File, hdr:String, sentsGloss:Array[String]):Unit = {
		startTrack("Document " + hdr)
		//--Create Document
		//(gloss)
		val docGloss:String = 
				sentsGloss.foldLeft(new StringBuilder){case (b:StringBuilder,l:String)=>
					b.append(l).append("\n"); b }.toString 
		//(process header)
		val date(year,month,day) = hdr
		val name(filename) = hdr
		val pubTime = new DateTime(year.toInt,month.toInt,day.toInt,0,0,0,0)
		//(debug)
		log("Processing " + filename)
		//--Create CoreMap
		//(create)
		val doc:Annotation = new Annotation(docGloss)
		//(set calendar)
		val ground:DateTime = new DateTime(year.toInt,month.toInt,day.toInt,0,0,0,0)
		doc.set(classOf[CalendarAnnotation], 
			ground.toGregorianCalendar.asInstanceOf[Calendar])
		//(annotate)
		pipeline.annotate(doc)
		log("(JavaNLP annotated)")
		//(gutime annotation)
		val gutime = new GUTimeAnnotator(new File(
			System.getenv("HOME")+"/workspace/time/etc/"))
		gutime.annotate(doc)
		log("(gutime annotated)")
		//--Iterate Timexes
		//(variables)
		var tid:Int = 0
		var sI:Int = 0
		var wI:Int = 0
		val sentsJ:JList[CoreMap] = doc.get[JList[CoreMap],SentencesAnnotation](
			classOf[SentencesAnnotation])
		val sents:Array[CoreMap] = sentsJ.toList.toArray
		val tokens:Array[Array[CoreLabel]] = sents.map{ (c:CoreMap) =>
				val j:JList[CoreLabel] =
					c.get[JList[CoreLabel],TokensAnnotation](classOf[TokensAnnotation])
				j.toList.toArray
			}
		//((serialization crap))
		doc.set[JList[CoreMap],JList[CoreMap],SentencesAnnotation](
			classOf[SentencesAnnotation],new ArrayList(sents.toList))
		(0 until sents.length).foreach{ (sid:Int) => 
			val j:JList[CoreLabel] = sents(sid).get[JList[CoreLabel],
				TokensAnnotation](classOf[TokensAnnotation])
			val s:List[CoreLabel] = j.toList
			val out:JList[CoreLabel] = new ArrayList(s)
			sents(sid).set(classOf[TokensAnnotation],out)
		}
		//((timexes))
		var timexes:List[CoreMap] = List[CoreMap]()
		//(iterate)
		startTrack("Timexes")
		doc.get[
				JList[CoreMap],TimexAnnotations](
				classOf[TimexAnnotations]).foreach{ case (map:CoreMap) =>
			try {
				//((get vars))
				val begin:Int 
					= map.get[java.lang.Integer,CharacterOffsetBeginAnnotation](
						classOf[CharacterOffsetBeginAnnotation])
				val end:Int 
					= map.get[java.lang.Integer,CharacterOffsetEndAnnotation](
						classOf[CharacterOffsetEndAnnotation])
				val timex:StanfordTimex = map.get[StanfordTimex,TimexAnnotation](
					classOf[TimexAnnotation])
				//((find beginning))
				while(tokens(sI)(wI).beginPosition < begin){
					wI += 1
					if(wI >= tokens(sI).length){ sI += 1; wI = 0 }
				}
				if(tokens(sI)(wI).beginPosition != begin){
					throw new IllegalStateException(
						"Begin is not on a word boundary: "+begin+" : " + tokens(sI)(wI))
				}
				val beginIndex = wI
				//((find end))
				while(tokens(sI)(wI).endPosition < end){
					wI += 1
					if(wI >= tokens(sI).length){ 
						throw new IllegalStateException("Timex crosses sentence boundary")
					}
				}
				if(tokens(sI)(wI).endPosition != end){
					throw new IllegalStateException(
						"End is not on a word boundary: "+end+" : " + tokens(sI)(wI))
				}
				val endIndex = wI+1
				//(process timex)
				//((vars))
				val timexValue = timex.value
				val timexType = timex.timexType
				if(timexValue != null){
					val timex:CoreMap = new ArrayCoreMap(4)
					//((fields))
					timex.set(classOf[TimeIdentifierAnnotation], "t"+tid)
					tid += 1
					timex.set(classOf[BeginIndexAnnotation], 
						new java.lang.Integer(beginIndex))
					timex.set(classOf[EndIndexAnnotation],
						new java.lang.Integer(endIndex))
					timex.set(classOf[OriginalTimeValueAnnotation], timexValue)
					timex.set(classOf[OriginalTimeTypeAnnotation], timexType)
					timex.set(classOf[TimeValueAnnotation], 
						DataLib.jodaTime2Array(
							DataLib.timex2JodaTime(timexValue,ground),
							timexValue)
						)
					//((save))
					if(!sents(sI).has[JList[CoreMap],TimeExpressionsAnnotation](
							classOf[TimeExpressionsAnnotation])){
						sents(sI).set(classOf[TimeExpressionsAnnotation],
							new ArrayList[CoreMap]().asInstanceOf[JList[CoreMap]])
					}
					val timexes = sents(sI).get[JList[CoreMap],TimeExpressionsAnnotation](
							classOf[TimeExpressionsAnnotation])
					timexes.add(timex)
					//((debug))
					log("added: " + 
						tokens(sI).slice(beginIndex,endIndex).map{ _.word }.mkString(" ") + 
						" ["+timexValue+"]")
				}
			} catch {
				case (e:RuntimeException) =>
					err("FAILED: " + e.getMessage)
			}
		}
		endTrack("Timexes")
		//(post-process)
		log("(tokenizing)")
		DataLib.retokenize(doc)
		log("(normalizing numbers)")
		DataLib.normalizeNumbers(doc)
		//(removing GuTime Timexes)
		log("(cleaning up)")
		doc.remove[StanfordTimex,TimexAnnotation](classOf[TimexAnnotation])
		doc.remove[JList[CoreLabel],TokensAnnotation](classOf[TokensAnnotation])
		doc.remove[String,TextAnnotation](classOf[TextAnnotation])
		sents.foreach{ (sent:CoreMap) => 
			sent.remove[JList[CoreMap],TimexAnnotations](classOf[TimexAnnotations])
		}
		//(write)
		log("(writing file)")
		val outFile = outDir.getPath + "/" + filename + ".coremap"
		try {
			IOUtils.writeObjectToFile(doc,outFile)
//			val fos = new FileOutputStream(outFile);
//			val out = new ObjectOutputStream(fos);
//			out.writeObject(doc);
//			out.close();
		} catch {
			case (e:IOException) => {
				log(e)
				new File(outFile).delete
				throw new RuntimeException(e);
			}
		}
		//(done)
		log("(ending track)")
		endTrack("Document " + hdr)
	}

	def appendFile(outDir:File,f:File):(Int,Int) = {
		import scala.io.Source.fromFile
		var lastDoc:String = ""
		var inSent:Boolean = false
		var sent:String = ""
		var sents:List[String] = List[String]()
		var success:Int = 0
		var total:Int = 0
		fromFile(f).getLines.foreach{ case (line:String) =>
			if(line.startsWith("<DOC id")){
				lastDoc = line
			} else{
				line.trim.toLowerCase match {
					case "<s>" => assert(!inSent); inSent = true; sent = ""
					case "</s>" => 
						assert(inSent); inSent = false; 
						sents = sent.replaceAll("""\s+"""," ") :: sents
					case "<text>" => //noop
					case "</text>" => //noop
					case "</doc>" => 
						try{
						appendDoc(outDir,f,lastDoc,sents.reverse.toArray); 
						} catch {
							case (e:Exception) => {}
						}
						sents = List[String]();
					case _ => sent = sent + line
				}
			}
		}
		(success,total)
	}

	def process(args:Array[String]) = {
		DateTimeZone.setDefault(DateTimeZone.UTC);
		//--Arguments
		//(error check)
		if(args.length != 2){
			err("usage: gigaword [outdir] [file]")
			System.exit(1)
		}
		val outDir = new File(args(0))
		val inFile = new File(args(1))
		if(!outDir.isDirectory){ 
			throw new IllegalArgumentException("not a directory: " + outDir)
		}
		if(inFile.isDirectory){ 
			throw new IllegalArgumentException("input is a directory: " + inFile)
		}
		//--Process File
		val (success,total) = appendFile(outDir,inFile)

	}
}
