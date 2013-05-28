package time

import java.lang.{Integer => JInt}
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
		b.append("""^\s*""")
		(0 until (n-1)).foreach{ (i:Int) => b.append("""([^\s]+)\s+""") }
		b.append("""([^\s]+)""")
		b.append("""\s*$""")
		b.toString.r
	}
	val Dct = mkline(2)
	val Base = mkline(4)
	val Attr = mkline(8)
	val Ext = mkline(6)
	val PubTime = """([0-9]{4})-?([0-9]{2})-?([0-9]{2})""".r
	val DctChinese = """^([^:]+)\.fid:<DATE>([^<]+)</DATE>$""".r

	//<<classes>
  // wordLookup is whatever indexing scheme the particular language is using
  // to find the word with
	case class TempEval2Word(doc:String,sent:Int,word:Int,wordLookup:Int) {
		var gloss:Option[String] = None
		def fill(g:String):TempEval2Word = {
			val rtn = TempEval2Word(doc,sent,word,wordLookup)
			rtn.gloss = Some(g)
			rtn.index = index
			rtn
		}
		var index:Option[Int] = None
		def index(i:Int):TempEval2Word = {
			val rtn = TempEval2Word(doc,sent,word,wordLookup)
			rtn.gloss = gloss
			rtn.index = Some(i)
			rtn
		}
    override def equals(o:Any):Boolean = {
      o match {
        case (w:TempEval2Word) =>
          return w.doc == doc && w.sent == sent && w.wordLookup == wordLookup
        case _ => return false
      }
    }
    override def hashCode:Int = {
      return doc.hashCode ^ sent.hashCode ^ wordLookup.hashCode
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
	def words(base:String, lang:String):Map[TempEval2Word,TempEval2Word] = {
		//(variables)
		val lines = 
      if (lang == "chinese") scala.io.Source.fromFile(base, "GB18030").mkString.split("""\s*\n\s*""")
      else scala.io.Source.fromFile(base, "utf-8").mkString.split("""\s*\n\s*""")
		val rtn = new HashMap[TempEval2Word,TempEval2Word]
		//(parse lines)
    var wordOfSent:Int = 0
    var sentI:Int      = -1
		lines.zipWithIndex.foreach{ case (line:String,i:Int) =>
			val Base(doc,sent,word,gloss) = line
      if (sent.toInt != sentI) {
        wordOfSent = 0
        sentI = sent.toInt
      }
//      if (lang != "italian") assert(wordOfSent == word.toInt)
			val te2Word = TempEval2Word(doc, sent.toInt,
                                  if(lang == "italian") wordOfSent else word.toInt,
                                  word.toInt)
				.fill(gloss)
				.index(i)
			assert(!rtn.contains(te2Word), "Duplicate word: " + te2Word)
			rtn(te2Word) = te2Word
      wordOfSent += 1
		}
		//(return)
		rtn
	}

	/**
		Document name to creation time
	*/
	def documentCreationTimes(dct:String, lang:String):Map[String,DateTime] = {
		//(variables)
		val lines = {
			try {
				scala.io.Source.fromFile(dct).mkString.split("""\s*\n\s*""")
			} catch {
				case (e:java.io.FileNotFoundException) =>
					scala.io.Source.fromFile(dct.replaceAll(".txt",".tab")).mkString.split("""\s*\n\s*""")
			}
		}
		val rtn = new HashMap[String,DateTime]
		//(parse lines)
		lines.foreach{ case (line:String) =>
      var doc = "";
      var dateStr = ""
      if (lang == "chinese") {
			  var DctChinese(docX,dateStrX) = line
        doc = docX; dateStr = dateStrX
      } else {
			  var Dct(docX,dateStrX) = line
        doc = docX; dateStr = dateStrX
      }
			val PubTime(year,month,day) = dateStr
			val date:DateTime = new DateTime(year.toInt,
                                       math.max(month.toInt, 1),
                                       math.max(day.toInt, 1),
                                       0,0,0,0)
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
			collect((TempEval2Word(doc,sent.toInt,start.toInt,start.toInt),'tid)) = tid
			collect((TempEval2Word(doc,sent.toInt,start.toInt,start.toInt),'doc)) = doc
			if(tag == "type"){
				collect((TempEval2Word(doc,sent.toInt,start.toInt,start.toInt),'type)) = value
			} else if(tag == "value" || tag == "val"){
				collect((TempEval2Word(doc,sent.toInt,start.toInt,start.toInt),'value)) = value
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
    log("Should collect " + rtn.size + " timexes")
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
		//(fill blanks)
		base.keys.foreach{ (word:TempEval2Word) =>
				rtn(word) = None
		}
		//(parse lines)
		lines.foreach{ case (line:String) =>
			val Ext(doc,sent,word,timex3,tid,one) = line
			val teWord = TempEval2Word(doc,sent.toInt,-1,word.toInt)
			rtn(teWord) = Some(tid)
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
			ext:Map[TempEval2Word,Option[String]],
			lang:String
			):Array[Annotation] = {
		startTrack("Making CoreMaps")
		//--Utilities
		case class SentenceInfo(sent:Int,doc:String) {
			var tokens = new HashMap[Int,CoreLabel]
			var textGloss = new HashMap[Int,String]
			var timeExpressions = List[CoreMap]()
			def mkGloss:String = {
				val tokenRange = if(lang.toLowerCase == "espanol") (1 to textGloss.keys.size)
			                   else (0 until textGloss.keys.size);
				tokenRange.map{ textGloss(_) }
					.toArray
					.mkString(" ")
			}
			def mkTokens:JList[CoreLabel] = {
				val tokenRange = if(lang.toLowerCase == "espanol") (1 to tokens.keys.size)
			                   else (0 until tokens.keys.size);
				seqAsJavaList(tokenRange.map{ tokens(_) })
			}
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
          if (attr.contains(AttributeKey(word.doc,word.sent,tid))) {
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
          } else {
            log("Could not find key: " + AttributeKey(word.doc,word.sent,tid))
          }
				case _ => // do nothing
			}
		}
		endTrack("Pass 1 (collect info)")
		//--Offsets + Expressions
		forceTrack("Pass 2 (offsets)")
		var offset:Int = 0
		var lastSent:Option[SentenceInfo] = None
		sentences.foreach{ case (doc:String,sent:Int) =>
			//(variables)
			val sentInfo = maps((doc,sent))
			var timexStart:Option[Int] = None
			var timexMap:Option[CoreMap] = None
			if(lastSent.isDefined && lastSent.get.doc != sentInfo.doc){
				offset = 0
			}
			//(cycle words)
			val tokenRange = if(lang.toLowerCase == "espanol") (1 to sentInfo.tokens.keys.size) 
			            else (0 until sentInfo.tokens.keys.size);
			tokenRange.foreach{ (wordI:Int) =>
				if (!sentInfo.tokens.contains(wordI)) {
					throw new IllegalArgumentException("Unknown word @ " + sentInfo.doc + " sentence " + sentInfo.sent + " word index " + wordI);
				}
				val wordInfo = sentInfo.tokens(wordI)
				val text:String = wordInfo.get[String,TextAnnotation](TEXT)
				//((offset))
				wordInfo.set(classOf[CharacterOffsetBeginAnnotation],
					new java.lang.Integer(offset))
				wordInfo.set(classOf[CharacterOffsetEndAnnotation],
					new java.lang.Integer(offset+text.length))
				offset += (text.length + 1)
				//((token offsets))
				wordInfo.set(classOf[TokenBeginAnnotation],
					new java.lang.Integer(wordI))
				wordInfo.set(classOf[TokenEndAnnotation],
					new java.lang.Integer(wordI+1))
			}
			lastSent = Some(sentInfo)
		}
		endTrack("Pass 2 (offsets)")
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
				val sentMap = new ArrayCoreMap(5)
				sentMap.set(classOf[TextAnnotation], sentInfo.mkGloss)
				sentMap.set(classOf[TokensAnnotation], sentInfo.mkTokens)
				sentMap.set(classOf[TimeExpressionsAnnotation], sentInfo.mkTimes)
				sentMap.set(classOf[CharacterOffsetBeginAnnotation], 
					sentInfo.tokens(if(lang == "espanol") 1 else 0).get[JInt,CharacterOffsetBeginAnnotation](
						classOf[CharacterOffsetBeginAnnotation]))
				sentMap.set(classOf[CharacterOffsetEndAnnotation], 
					sentInfo.tokens(sentInfo.tokens.keySet.size-1).get[JInt,CharacterOffsetEndAnnotation](
						classOf[CharacterOffsetEndAnnotation]))
				sentMap
			}
			assertValidSentence(sent)
			val sentences:JList[CoreMap] 
				= map.get[JList[CoreMap],SentencesAnnotation](
					classOf[SentencesAnnotation])
			sentences.add(sent)
			//(text)
			val text:String = map.get[String,TextAnnotation](classOf[TextAnnotation])
//			if(!text.endsWith(sentInfo.mkGloss)){
				map.set(classOf[TextAnnotation],
					{if(text != "") text+"\n" else "" } + sentInfo.mkGloss)
//			}
			//(isText)
			map.set(classOf[IsTestAnnotation],test)
		}
		endTrack("Pass 3 (make coremaps)")
		//--Relink
		forceTrack("Pass 4 (global timexes)")
    var timexes_added = 0
		docs.foreach{ (doc:Annotation) =>
      // Annotate
      var timexes_added_to_doc = 0
      var sentI:Int = 0
			doc.get[java.util.List[CoreMap],SentencesAnnotation](SENTENCES)
					.foreach{ (sent:CoreMap) =>
				val added = DataLib.relinkTimexes(sent)
        timexes_added_to_doc += added
        sentI += 1;
			}
      timexes_added += timexes_added_to_doc
		}
    log("" + timexes_added + " timexes registered")
		endTrack("Pass 4 (global timexes)")
		//--Pipeline Annotate
//		forceTrack("Pass 5 (pipeline)")
//		//(annotators)
//		val lemma = new MorphaAnnotator(false)
//		val pos = new POSTaggerAnnotator(
//				System.getenv("HOME") +
//					"/lib/data/bidirectional-distsim-wsj-0-18.tagger", //TODO hard coded
//				false)
//		//(run pipeline)
//		threadAndRun( 
//			docs.zipWithIndex.map{ case (doc:Annotation,i:Int) => new Runnable{ 
//				override def run {
//					pos.annotate(doc)
//					lemma.annotate(doc)
//					log("("+(i+1)+"/"+docs.length+ ") Annotated " 
//						+ doc.get[String,DocIDAnnotation](classOf[DocIDAnnotation]))
//				}
//			} }
//		)
//		endTrack("Pass 5 (pipeline)")
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
	class TempEval2RetokTask {
		def perform(d:Dataset[CoreMapDatum]):Unit = {
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
	}
	
	class TempEval2NumberNormalizeTask {
		def perform(d:Dataset[CoreMapDatum]):Unit = {
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
		val words:Map[TempEval2Word,TempEval2Word] = this.words(basePath, lang)
		log("document creation times...")
		val dct:Map[String,DateTime] = this.documentCreationTimes(dctPath, lang)
		log("attributes...")
		val attr:Map[AttributeKey,RawTime] = this.attr(attrPath)
		log("extents...")
		val ext:Map[TempEval2Word,Option[String]] = this.ext(words,extPath)
		//--CoreMap
		val maps:Array[Annotation] = mkCoreMaps(test,words,dct,attr,ext,lang)
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
		val trainDat = mkData(lang,false,baseTrain,dctTrain,attrTrain,extTrain)
		log(FORCE, "Train Timexes: " + 
			(new TimeDataset(new SerializedCoreMapDataset("/tmp/a.ser.gz",trainDat))
				.goldSpans(true)).length)
		val testDat  = mkData(lang,true,baseTest,dctTest,attrTest,extTest)
		log(FORCE, "Test Timexes: " + 
			(new TimeDataset(new SerializedCoreMapDataset("/tmp/b.ser.gz",testDat))
				.goldSpans(false)).length)
		//--Combine
		val rtn = new SerializedCoreMapDataset(
			"aux/coremap/tempeval2-"+lang, Array.concat(trainDat, testDat))
		//--Return
		endTrack("Making TempEval2")
		rtn
	}

	//<<retok>>
	def retokFromInit(init:SerializedCoreMapDataset, lang:String
			):SerializedCoreMapDataset = {
		init.saveAs("aux/coremap/tempeval2-"+lang+"-retok")
		(new TempEval2RetokTask).perform(init)
		init.save
		init
	}
	def retokFromInit(source:String, lang:String):SerializedCoreMapDataset = {
		retokFromInit( new SerializedCoreMapDataset(source), lang )
	}
	def retok(dir:String,lang:String):SerializedCoreMapDataset = {
		retokFromInit( apply(dir,lang), lang )
	}

	//<<normalize>>
	def normalizeFromRetok(retok:SerializedCoreMapDataset, lang:String
			):SerializedCoreMapDataset = {
		retok.saveAs("aux/coremap/tempeval2-"+lang+"-retok-numbers")
		(new TempEval2NumberNormalizeTask).perform(retok)
		retok.save
		retok
	}
	def normalizeFromRetok(source:String, lang:String):SerializedCoreMapDataset = {
		normalizeFromRetok( new SerializedCoreMapDataset(source), lang )
	}
	def normalizeFromInit(source:String, lang:String):SerializedCoreMapDataset = {
		normalizeFromRetok( retokFromInit(source, lang), lang )
	}
	def normalizeFromInit(init:SerializedCoreMapDataset, lang:String
			):SerializedCoreMapDataset = {
		normalizeFromRetok( retokFromInit(init, lang), lang )
	}
	def normalize(dir:String,lang:String):SerializedCoreMapDataset = {
		val x = normalizeFromInit( apply(dir, lang), lang )
		x
	}

	def toReadableFile(input:String, output:String):Unit = {
		val data = new SerializedCoreMapDataset(input)
		val contents = new StringBuilder();
    def write(typ:String, value:String, date:String, text:String) = {
		  contents.append(typ).append("\t").append(value).append("\t")
		  	.append(date).append("\t")
		  	.append(text).append("\n")
    }
		//--Info
		var words = List[String]()
		var currentValue = ""
		var currentType = ""
		//--Iterate Over Data
		data.foreach{ (datum:CoreMapDatum) =>
		  var currentId = ""
			val cal = datum.get[java.util.Calendar,CalendarAnnotation](classOf[CalendarAnnotation])
			val format = new java.text.SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
			val docdate = format.format(cal.getTime)
			val sentences = datum.get[JList[CoreMap],SentencesAnnotation](classOf[SentencesAnnotation])
			sentences.foreach{ (sent:CoreMap) =>
				val tokens = sent.get[JList[CoreLabel],TokensAnnotation](classOf[TokensAnnotation])
				//--Iterate Over Tokens
				tokens.foreach{ (token:CoreLabel) =>
					if (token.get[String,OriginalTimeValueAnnotation](classOf[OriginalTimeValueAnnotation]) != null) {
						//(get info)
						val word = token.get[String,TextAnnotation](classOf[TextAnnotation])
						val value = token.get[String,OriginalTimeValueAnnotation](classOf[OriginalTimeValueAnnotation])
						val typ = token.get[String,OriginalTimeTypeAnnotation](classOf[OriginalTimeTypeAnnotation])
						val id = token.get[String,TimeIdentifierAnnotation](classOf[TimeIdentifierAnnotation])
            if (id != null && currentId != null && id == currentId) {
              //(case: still in a time)
              words = word :: words
            }
            if (id == null && currentId != "") {
              //(case: left a time)
              write(currentType, currentValue, docdate, words.reverse.mkString(" "))
              currentId = ""
            }
            if (currentId == "" && id != null) {
              //(case: entered a time)
              words = List[String](word)
              currentId = id;
							currentValue = value
							currentType = typ
            }
            if (id != null && currentId != "" && id != currentId) {
              //(case: switching times)
              write(currentType, currentValue, docdate, words.reverse.mkString(" "))
              words = List[String](word)
              currentId = id;
							currentValue = value
							currentType = typ
            }
					}
				}
			}
      if (currentId != "") {
        //(case: left a time)
        write(currentType, currentValue, docdate, words.reverse.mkString(" "))
        currentId = ""
      }
		}
		edu.stanford.nlp.io.IOUtils.writeStringToFileNoExceptions(
			contents.toString, output, "UTF-8");
	}

	def main(args:Array[String]) {
//    // Print a serialized core map
//		if(args.length > 0){
//			if(args(0) == "english"){
//				prettyLog(new SerializedCoreMapDataset(
//					"aux/coremap/tempeval2-english"))
//			} else if(args(0) == "retok"){
//				prettyLog(new SerializedCoreMapDataset(
//					"aux/coremap/tempeval2-english-retok"))
//			} else if(args(0) == "numbers"){
//				prettyLog(new SerializedCoreMapDataset(
//					"aux/coremap/tempeval2-english-retok-numbers"))
//			} else {
//				throw new IllegalArgumentException("Bad argument: " + args(0))
//			}
//		} else {
//			prettyLog(new SerializedCoreMapDataset(
//				"aux/coremap/tempeval2-english-retok-numbers"))
//		}
		// Dump a language's data into readable form
		val language = args(0)
		println("Processing " + language)
		val init = apply("aux/tempeval2-cleaned", language)
                val retok = retokFromInit(init, language)
                normalizeFromRetok(retok, language)
		toReadableFile(
			"aux/coremap/tempeval2-"+language,
			"tmp/tempeval-"+language+".dat"
		)
	}
}
