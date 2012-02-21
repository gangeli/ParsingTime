package time

import java.util.{List => JList}
import java.util.ArrayList
import java.util.Calendar;
import java.util.GregorianCalendar;

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import org.joda.time._

import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.util.logging.Redwood.RedwoodChannels
import edu.stanford.nlp.util.logging.PrettyLoggable
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.util._
import edu.stanford.nlp.time.{Timex => StanfordTimex}
import edu.stanford.nlp.time.TimeAnnotations._

import org.goobs.stanford.JavaNLP._
import org.goobs.stanford._
import org.goobs.testing.Dataset

//------------------------------------------------------------------------------
// TempEval2 Data
//------------------------------------------------------------------------------

object TempEval2 {
	//----------
	// UTILITIES
	//----------
	//<<regexes>>
	def mkline(n:Int) = {
		val b = new StringBuilder
		b.append("""\s*""")
		(0 until (n-1)).foreach{ (i:Int) => b.append("""([^\s]+)\s+""") }
		b.append("""([^\s]+)""")
		b.append("""\s*""")
		b.toString.r
	}
	val Dct = mkline(2)
	val Base = mkline(4)
	val Attr = mkline(8)
	val Ext = mkline(6)
	val PubTime = """([0-9]{4})([0-9]{2})([0-9]{2})""".r

	//<<classes>
	case class TempEval2Word(doc:String,sent:Int,word:Int) {
		var gloss:Option[String] = None
		def fill(g:String):TempEval2Word = {
			val rtn = TempEval2Word(doc,sent,word)
			rtn.gloss = Some(g)
			rtn.index = index
			rtn
		}
		var index:Option[Int] = None
		def index(i:Int):TempEval2Word = {
			val rtn = TempEval2Word(doc,sent,word)
			rtn.gloss = gloss
			rtn.index = Some(i)
			rtn
		}
	}
	case class RawTime(tid:String,timexType:String,timexValue:String)
	case class AttributeKey(doc:String,sent:Int,tid:String)

	//----------
	// PROCESSING
	//----------
	/**
		Word to word with gloss
	*/
	def words(base:String):Map[TempEval2Word,TempEval2Word] = {
		//(variables)
		val lines = scala.io.Source.fromFile(base).mkString.split("""\s*\n\s*""")
		val rtn = new HashMap[TempEval2Word,TempEval2Word]
		//(parse lines)
		lines.zipWithIndex.foreach{ case (line:String,i:Int) =>
			val Base(doc,sent,word,gloss) = line
			val te2Word = TempEval2Word(doc,sent.toInt,word.toInt)
				.fill(gloss)
				.index(i)
			assert(!rtn.contains(te2Word), "Duplicate word: " + te2Word)
			rtn(te2Word) = te2Word
		}
		//(return)
		rtn
	}

	/**
		Document name to creation time
	*/
	def documentCreationTimes(dct:String):Map[String,DateTime] = {
		//(variables)
		val lines = scala.io.Source.fromFile(dct).mkString.split("""\s*\n\s*""")
		val rtn = new HashMap[String,DateTime]
		//(parse lines)
		lines.foreach{ case (line:String) =>
			val Dct(doc,dateStr) = line
			val PubTime(year,month,day) = dateStr
			val date:DateTime = new DateTime(year.toInt,month.toInt,day.toInt,0,0,0,0)
			rtn(doc) = date
		}
		//(return)
		rtn
	}

	/**
		Word to raw timex information
	*/
	def attr(attr:String):Map[AttributeKey,RawTime] = {
		//--Variables
		val lines = scala.io.Source.fromFile(attr).mkString.split("""\s*\n\s*""")
		val collect = new HashMap[(TempEval2Word,Symbol),String]
		val rtn = new HashMap[AttributeKey,RawTime]
		//--Collect Pass
		lines.foreach{ case (line:String) =>
			//(variables)
			val Attr(doc,sent,start,timex3,tid,junk,tag,value) = line
			//(set attributes)
			collect((TempEval2Word(doc,sent.toInt,start.toInt),'tid)) = tid
			collect((TempEval2Word(doc,sent.toInt,start.toInt),'doc)) = doc
			if(tag == "type"){
				collect((TempEval2Word(doc,sent.toInt,start.toInt),'type)) = value
			} else if(tag == "value"){
				collect((TempEval2Word(doc,sent.toInt,start.toInt),'value)) = value
			} else {
				throw new IllegalStateException("bad tag: " + tag)
			}
		}
		//--Save Pass
		collect
			.filter{ case ((w:TempEval2Word,tag:Symbol),v:String) => tag == 'tid }
			.foreach{ case ((word:TempEval2Word,tag:Symbol),value:String) => 
				assert(tag == 'tid)
				//(variables)
				val tid:String = value
				val timexTyp = collect((word,'type))
				val timexVal = collect((word,'value))
				//(save)
				rtn(AttributeKey(word.doc,word.sent,tid)) 
					= RawTime(tid,timexTyp,timexVal)
			}
		//--Return
		rtn
	}

	/**
		Word to (possible) timex tag
	*/
	def ext(base:Map[TempEval2Word,TempEval2Word],ext:String
			):Map[TempEval2Word,Option[String]] = {
		//(variables)
		val lines = scala.io.Source.fromFile(ext).mkString.split("""\s*\n\s*""")
		val rtn = new HashMap[TempEval2Word,Option[String]]
		//(parse lines)
		lines.foreach{ case (line:String) =>
			val Ext(doc,sent,word,timex3,tid,one) = line
			val teWord = TempEval2Word(doc,sent.toInt,word.toInt)
			rtn(teWord) = Some(tid)
		}
		//(fill blanks)
		base.keys.foreach{ (word:TempEval2Word) =>
			if(!rtn.contains(word)){
				rtn(word) = None
			}
		}
		//(return)
		rtn
		
	}
	
	//----------
	// To Core Map
	//----------
	def mkCoreMaps(
			test:Boolean,
			words:Map[TempEval2Word,TempEval2Word],
			dct:Map[String,DateTime],
			attr:Map[AttributeKey,RawTime],
			ext:Map[TempEval2Word,Option[String]]
			):Array[Annotation] = {
		startTrack("Making CoreMaps")
		//--Utilities
		case class SentenceInfo(sent:Int,doc:String) {
			var tokens = new HashMap[Int,CoreLabel]
			var textGloss = new HashMap[Int,String]
			var timeExpressions = List[CoreMap]()
			def mkGloss:String
				= (0 until textGloss.keys.size).map{ textGloss(_) }
					.toArray
					.mkString(" ")
			def mkTokens:JList[CoreLabel]
				= seqAsJavaList((0 until tokens.keys.size).map{ tokens(_) })
			def mkTimes:JList[CoreMap] = seqAsJavaList(timeExpressions)
		}
		//--Variables
		val maps = new HashMap[(String,Int),SentenceInfo]
		val sentences = words.values.toArray
			.sortWith{ words(_).index.get < words(_).index.get }
			.map{ x => (x.doc,x.sent) }
			.distinct
		//--Collect Info
		forceTrack("Pass 1 (collect info)")
		words.values.foreach{ (word:TempEval2Word) =>
			//(ensure sentence info)
			if(!maps.contains((word.doc,word.sent))){ 
				maps((word.doc,word.sent)) = SentenceInfo(word.sent,word.doc) 
			}
			val sentInfo = maps((word.doc,word.sent))
			//(variables)
			val ground:DateTime = dct(word.doc)
			//(update token)
			sentInfo.tokens(word.word) = new CoreLabel(7)
			val wordInfo = sentInfo.tokens(word.word)
			wordInfo.set(classOf[AfterAnnotation]," ")
			wordInfo.set(classOf[BeforeAnnotation]," ")
			wordInfo.set(classOf[OriginalTextAnnotation],word.gloss.get)
			wordInfo.set(classOf[TextAnnotation],word.gloss.get)
			wordInfo.set(classOf[ValueAnnotation],word.gloss.get)
			//(update text)
			assert(!sentInfo.textGloss.contains(word.word))
			sentInfo.textGloss(word.word) = word.gloss.get
			//(update local timex)
			ext(word) match {
				case Some(tid) =>
					val raw = attr(AttributeKey(word.doc,word.sent,tid))
					wordInfo.set(classOf[OriginalTimeMetaAnnotation],
						TimexMetaInfo(word.doc,word.sent,word.word,raw.tid))
					wordInfo.set(classOf[OriginalTimeTypeAnnotation],
						raw.timexType)
					wordInfo.set(classOf[OriginalTimeValueAnnotation],
						raw.timexValue)
					wordInfo.set(classOf[TimeIdentifierAnnotation],
						raw.tid)
					wordInfo.set(classOf[TimeValueAnnotation],
						DataLib.jodaTime2Array(
							DataLib.timex2JodaTime(raw.timexValue,ground),
							raw.timexValue)
						)
				case _ => //do nothing
			}
		}
		endTrack("Pass 1 (collect info)")
		//--Offsets + Expressions
		forceTrack("Pass 2 (offset + global timexes)")
		var offset:Int = 0
		var lastSent:Option[SentenceInfo] = None
		sentences.foreach{ case (doc:String,sent:Int) =>
			//(variables)
			val sentInfo = maps((doc,sent))
			var timexStart:Option[Int] = None
			var timexMap:Option[CoreMap] = None
			if(true || lastSent.isDefined && lastSent.get.doc != sentInfo.doc){
				offset = 0
			}
			//(cycle words)
			(0 until sentInfo.tokens.keys.size).foreach{ (wordI:Int) =>
				val wordInfo = sentInfo.tokens(wordI)
				val text:String = wordInfo.get[String,TextAnnotation](TEXT)
				//((offset))
				wordInfo.set(classOf[CharacterOffsetBeginAnnotation],
					new java.lang.Integer(offset))
				wordInfo.set(classOf[CharacterOffsetEndAnnotation],
					new java.lang.Integer(offset+text.length))
				offset += (text.length + 1)
				//((timex))
				if(wordInfo.get[Array[String],TimeValueAnnotation](
						classOf[TimeValueAnnotation]) != null){
					//(((case: in time)))
					timexStart = timexStart.orElse(Some(wordI))
					timexMap = timexMap.orElse(Some(wordInfo))
				} else {
					//(((case: not in time)))
					timexStart match {
						case Some(start) =>
							val last = timexMap.get
							//((((copy coremap))))
							val time = new ArrayCoreMap(7)
							time.set(classOf[BeginIndexAnnotation],
								new java.lang.Integer(start))
							time.set(classOf[EndIndexAnnotation],
								new java.lang.Integer(wordI))
							time.set(classOf[OriginalTimeMetaAnnotation],
								last.get[TimexMetaInfo,OriginalTimeMetaAnnotation]
									(classOf[OriginalTimeMetaAnnotation]))
							time.set(classOf[OriginalTimeTypeAnnotation],
								last.get[String,OriginalTimeTypeAnnotation]
									(classOf[OriginalTimeTypeAnnotation]))
							time.set(classOf[OriginalTimeValueAnnotation],
								last.get[String,OriginalTimeValueAnnotation]
									(classOf[OriginalTimeValueAnnotation]))
							time.set(classOf[TimeIdentifierAnnotation],
								last.get[String,TimeIdentifierAnnotation]
									(classOf[TimeIdentifierAnnotation]))
							time.set(classOf[TimeValueAnnotation],
								last.get[Array[String],TimeValueAnnotation]
									(classOf[TimeValueAnnotation]))
							//((((save coremap))))
							sentInfo.timeExpressions = time :: sentInfo.timeExpressions
							//((((reset state))))
							timexStart = None
							timexMap = None
						case _ =>
					}
				}
			}
		}
		endTrack("Pass 2 (offset + global timexes)")
		//--To CoreMap
		forceTrack("Pass 3 (make coremaps)")
		//(variables)
		var docs = List[Annotation]()
		var gloss:String = ""
		//(loop)
		sentences.foreach{ case (doc:String,sentI:Int) =>
			val sentInfo = maps((doc,sentI))
			val map 
				= if(docs.length > 0 &&
							docs.head.get[String,DocIDAnnotation](classOf[DocIDAnnotation])
							== sentInfo.doc){
						//((case: keep document))
						docs.head
					} else {
						//((case: new document))
						val map = new Annotation("") //<--text annotation
						map.set(classOf[SentencesAnnotation], 
							new ArrayList[CoreMap]().asInstanceOf[JList[CoreMap]])
						//((id))
						map.set(classOf[IDAnnotation], ""+docs.length)
						//((add))
						docs = map :: docs
						map
					}
			//(calendar)
			map.set(classOf[CalendarAnnotation],
				dct(sentInfo.doc).toGregorianCalendar.asInstanceOf[Calendar])
			//(docID)
			map.set(classOf[DocIDAnnotation],sentInfo.doc)
			//(sentences)
			val sent:CoreMap = {
				val sentMap = new ArrayCoreMap(3)
				sentMap.set(classOf[TextAnnotation], sentInfo.mkGloss)
				sentMap.set(classOf[TokensAnnotation], sentInfo.mkTokens)
				sentMap.set(classOf[TimeExpressionsAnnotation], sentInfo.mkTimes)
				sentMap
			}
			assertValidSentence(sent)
			val sentences:JList[CoreMap] 
				= map.get[JList[CoreMap],SentencesAnnotation](
					classOf[SentencesAnnotation])
			sentences.add(sent)
			//(text)
			val text:String = map.get[String,TextAnnotation](classOf[TextAnnotation])
			if(!text.endsWith(sentInfo.mkGloss)){
				map.set(classOf[TextAnnotation],
					{if(text != "") text+"\n" else "" } + sentInfo.mkGloss)
			}
			//(isText)
			map.set(classOf[IsTestAnnotation],test)
		}
		endTrack("Pass 3 (make coremaps)")
		//--Pipeline Annotate
		forceTrack("Pass 4 (pipeline)")
		//(annotators)
		val lemma:Long=>MorphaAnnotator = {
			val cache = HashMap[Long,MorphaAnnotator]()
			(thread:Long) => {
				if(!cache.contains(thread)){
					cache(thread) = new MorphaAnnotator(false)
				}
				cache(thread)
			}
		}
		val pos:Long=>POSTaggerAnnotator = {
			val cache = HashMap[Long,POSTaggerAnnotator]()
			(thread:Long) => {
				if(!cache.contains(thread)){
					cache(thread) = new POSTaggerAnnotator(
						System.getenv("HOME") +
							"/lib/data/bidirectional-distsim-wsj-0-18.tagger", //TODO hard coded
						false)
				}
				cache(thread)
			}
		}
		//(run pipeline)
		threadAndRun( 
			docs.zipWithIndex.map{ case (doc:Annotation,i:Int) => new Runnable{ 
				override def run {
					pos(Thread.currentThread.getId).annotate(doc)
					lemma(Thread.currentThread.getId).annotate(doc)
					log("["+Thread.currentThread.getId+"]("+(i+1)+"/"+docs.length
						+ ") Annotated " 
						+ doc.get[String,DocIDAnnotation](classOf[DocIDAnnotation]))
				}
			} }
		)
		endTrack("Pass 4 (pipeline)")
		//--Return
		endTrack("Making CoreMaps")
		docs.reverse.toArray
	}

	//----------
	// TASKS
	//----------
	def assertValidSentence(sent:CoreMap) = {
		//(variables)
		val tokens = sent.get[JList[CoreLabel],TokensAnnotation](TOKENS)
		val text:String = sent.get[String,TextAnnotation](TEXT)
		val firstOffset = tokens.get(0).beginPosition
		//(check)
		tokens.foreach{ (tok:CoreLabel) =>
			val begin = tok.beginPosition-firstOffset
			val end = tok.endPosition-firstOffset
			val word = tok.originalText
			val check = text.substring(begin,end)
			assert(tok.word != null && tok.word != "")
			if(!word.equals(check)){
				prettyLog(sent)
			}
			assert(word.equals(check), "Word '" + word + "' maps to '" + check + "'")
		}
	}
	class TempEval2RetokTask extends Task[CoreMapDatum] {
		override def perform(d:Dataset[CoreMapDatum]):Unit = {
			forceTrack("Retokenizing")
			//(for each document)
			(0 until d.numExamples).foreach{ case (docIndex:Int) =>
				val sents=d.get(docIndex).get[JList[CoreMap],SentencesAnnotation](
					SENTENCES)
				//(check validity before)
				sents.foreach{ case (sent:CoreMap) =>
					assertValidSentence(sent)
				}
				//(retokenize)
				DataLib.retokenize(d.get(docIndex))
				//(check validity after)
				sents.foreach{ case (sent:CoreMap) =>
					assertValidSentence(sent)
				}
			}
			endTrack("Retokenizing")
		}
		override def dependencies = Array[Class[_ <: Task[_]]]()
		override def name = "retok"
	}
	
	class TempEval2NumberNormalizeTask extends Task[CoreMapDatum] {
		override def perform(d:Dataset[CoreMapDatum]):Unit = {
			forceTrack("Normalizing Numbers")
			//(for each document)
			(0 until d.numExamples).foreach{ case (docIndex:Int) =>
				DataLib.normalizeNumbers(d.get(docIndex))
				//(check validity)
				val sents=d.get(docIndex).get[java.util.List[CoreMap],
					SentencesAnnotation](SENTENCES)
				sents.foreach{ case (sent:CoreMap) =>
					assertValidSentence(sent)
				}
			}
			endTrack("Normalizing Numbers")
		}
		override def dependencies = Array[Class[_ <: Task[_]]]()
		override def name = "numbers"
	}
	
	
	//----------
	// TOP
	//----------
	def mkData(lang:String,test:Boolean,
			basePath:String,dctPath:String,attrPath:String,extPath:String
			):Array[Annotation] = {
		startTrack("Data(" + {if(test) "test" else "train"}+")")
		//--Data
		log("words...")
		val words:Map[TempEval2Word,TempEval2Word] = this.words(basePath)
		log("document creation times...")
		val dct:Map[String,DateTime] = this.documentCreationTimes(dctPath)
		log("attributes...")
		val attr:Map[AttributeKey,RawTime] = this.attr(attrPath)
		log("extents...")
		val ext:Map[TempEval2Word,Option[String]] = this.ext(words,extPath)
		//--CoreMap
		val maps:Array[Annotation] = mkCoreMaps(test,words,dct,attr,ext)
		//--Return
		endTrack("Data(" + {if(test) "test" else "train"}+")")
		maps
	}

	//<<init>>
	def apply(file:String):SerializedCoreMapDataset
	 	= new SerializedCoreMapDataset(file)
	def apply(dir:String,lang:String):SerializedCoreMapDataset = {
		startTrack("Making TempEval2")
		//--Locations
		val train     = dir+"/training/"+lang+"/data"
		val test      = dir+"/test/"+lang+"/key"
		val baseTrain = train+"/base-segmentation.tab"
		val baseTest  = test+"/base-segmentation.tab"
		val dctTrain  = train+"/dct.txt"
		val dctTest   = dir+"/test/"+lang+"/dct-"+lang.substring(0,2)+".txt"
		val attrTrain = train+"/timex-attributes.tab"
		val attrTest  = test+"/timex-attributes.tab"
		val extTrain  = train+"/timex-extents.tab"
		val extTest   = test+"/timex-extents.tab"
		//--Data
		val rtn = new SerializedCoreMapDataset(
			"aux/coremap/tempeval2-"+lang,
			Array.concat(
				mkData(lang,false,baseTrain,dctTrain,attrTrain,extTrain) ++ 
				mkData(lang,true,baseTest,dctTest,attrTest,extTest)
			)
		)
//		prettyLog(rtn)
		//--Return
		endTrack("Making TempEval2")
		rtn
	}

	//<<retok>>
	def retokFromInit(init:SerializedCoreMapDataset
			):SerializedCoreMapDataset = {
		init.runAndRegisterTask( new TempEval2RetokTask )
	}
	def retokFromInit(source:String):SerializedCoreMapDataset = {
		retokFromInit( new SerializedCoreMapDataset(source) )
	}
	def retok(dir:String,lang:String):SerializedCoreMapDataset = {
		retokFromInit( apply(dir,lang) )
	}

	//<<normalize>>
	def normalizeFromRetok(retok:SerializedCoreMapDataset
			):SerializedCoreMapDataset = {
		retok.runAndRegisterTask( new TempEval2NumberNormalizeTask )
	}
	def normalizeFromRetok(source:String):SerializedCoreMapDataset = {
		normalizeFromRetok( new SerializedCoreMapDataset(source) )
	}
	def normalizeFromInit(source:String):SerializedCoreMapDataset = {
		normalizeFromRetok( retokFromInit(source) )
	}
	def normalizeFromInit(init:SerializedCoreMapDataset
			):SerializedCoreMapDataset = {
		normalizeFromRetok( retokFromInit(init) )
	}
	def normalize(dir:String,lang:String):SerializedCoreMapDataset = {
		val x = normalizeFromInit( apply(dir,lang) )
		x
	}

	def main(args:Array[String]) {
		prettyLog(new SerializedCoreMapDataset(
			"aux/coremap/tempeval2-english-retok-numbers"))
	}
}
