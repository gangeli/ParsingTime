package time

import java.util.Calendar
import java.util.{List => JList}

import scala.util.Sorting.quickSort
import scala.collection.JavaConversions._

import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.ling.CoreAnnotation
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.util.logging.Redwood.Util._

import org.joda.time.DateTime
import org.joda.time.Period
import org.joda.time.DateTimeZone

import org.goobs.database._
import org.goobs.stanford.CoreMapDatum
import org.goobs.testing.Dataset

//------------------------------------------------------------------------------
// UTILITIES
//------------------------------------------------------------------------------
// -- ENUMERATIONS --
object Language extends Enumeration {
	val english = Value
}
object NumberType extends Enumeration {
	val NONE, ORDINAL, NUMBER, UNIT = Value
}

// -- ANNOTATIONS --
class TimeExpressionsAnnotation extends CoreAnnotation[java.util.List[CoreMap]]{
	def getType:Class[java.util.List[CoreMap]] = classOf[java.util.List[CoreMap]]
}
class TimeValueAnnotation extends CoreAnnotation[Array[String]]{
	def getType:Class[Array[String]] = classOf[Array[String]]
}
class OriginalTimeTypeAnnotation extends CoreAnnotation[String]{
	def getType:Class[String] = classOf[String]
}
class OriginalTimeValueAnnotation extends CoreAnnotation[String]{
	def getType:Class[String] = classOf[String]
}
class TimeIdentifierAnnotation extends CoreAnnotation[String]{
	def getType:Class[String] = classOf[String]
}
class IsTestAnnotation extends CoreAnnotation[Boolean]{
	def getType:Class[Boolean] = classOf[Boolean]
}
class OriginalTokensAnnotation 
		extends CoreAnnotation[java.util.List[CoreLabel]]{
	def getType:Class[java.util.List[CoreLabel]] 
		= classOf[java.util.List[CoreLabel]]
}
class OriginalBeginIndexAnnotation
		extends CoreAnnotation[java.lang.Integer]{
	def getType:Class[java.lang.Integer] = classOf[java.lang.Integer]
}
class OriginalEndIndexAnnotation
		extends CoreAnnotation[java.lang.Integer]{
	def getType:Class[java.lang.Integer] = classOf[java.lang.Integer]
}



//------------------------------------------------------------------------------
// DATA
//------------------------------------------------------------------------------
class TimeDataset(data:Dataset[CoreMapDatum]) {
	def slice(minInclusive:Int,maxExclusive:Int):TimeDataset
		= new TimeDataset(data.slice(minInclusive,maxExclusive))

	def timexes:Array[Timex] = {
		forceTrack("Reading Timexes")
		//(variables)
		var rtn:List[Timex] = List[Timex]()
		var index = 0
		//(get timexes)
		data.iterator.foreach{ (doc:CoreMapDatum) => //for each doc
			//(pub time)
			val pubTime = Time(new DateTime(
				doc.get[Calendar,CalendarAnnotation](classOf[CalendarAnnotation])
				.getTimeInMillis))
			//(is test)
			val isTest = 
				doc.get[Boolean,IsTestAnnotation](classOf[IsTestAnnotation])
			//(sentences)
			val sents = 
				doc.get[JList[CoreMap],SentencesAnnotation](
				classOf[SentencesAnnotation])
			sents.foreach{ (sent:CoreMap) => //for each sentence
				//(get tokens)
				val tokens:JList[CoreLabel] =
					sent.get[JList[CoreLabel],TokensAnnotation](
					classOf[TokensAnnotation])
				val origTokens:JList[CoreLabel] =
					sent.get[JList[CoreLabel],OriginalTokensAnnotation](
					classOf[OriginalTokensAnnotation])
				assert(tokens != null, "No tokens for " + sent)
				val tokenList:List[CoreLabel] = tokens.map{ x => x }.toList
				val origTokenList:List[CoreLabel] = origTokens.map{ x => x }.toList
				//(get timexes)
				val timexes:JList[CoreMap] = 
					sent.get[java.util.List[CoreMap],TimeExpressionsAnnotation](
					classOf[TimeExpressionsAnnotation])
				//(store timexes)
				if(timexes != null){
					timexes.foreach{ (timex:CoreMap) =>  //for each timex
						val tmx = 
							new Timex(index,timex,origTokenList,tokenList,pubTime,isTest)
						rtn = tmx :: rtn
						index += 1
						log(tmx)
					}
				}
			}
		}
		endTrack("Reading Timexes")
		//(return)
		rtn.reverse.toArray
	}
}

case class Timex(index:Int,time:CoreMap,origSent:List[CoreLabel],
		sent:List[CoreLabel],pubTime:Time,isTest:Boolean) {
	private val span:List[CoreLabel] = {
		val begin = time.get[java.lang.Integer,BeginIndexAnnotation](
							classOf[BeginIndexAnnotation])
		val end = time.get[java.lang.Integer,EndIndexAnnotation](
							classOf[EndIndexAnnotation])
		val origBegin = time.get[java.lang.Integer,OriginalBeginIndexAnnotation](
							classOf[OriginalBeginIndexAnnotation])
		val origEnd = time.get[java.lang.Integer,OriginalEndIndexAnnotation](
							classOf[OriginalEndIndexAnnotation])
		if(end <= begin){
			warn("Retokenized range is invalid: " + begin + " to " + end)
			assert(origBegin < origEnd, "Range is invalid: " + time)
			origSent.slice(origBegin,origEnd)
		} else {
			sent.slice(begin,end)
		}
	}

	private def numType(str:String):NumberType.Value = {
		if(str != null){
			str match {
				case "ORDINAL" => NumberType.ORDINAL
				case "NUMBER" => NumberType.NUMBER
				case "UNIT" => NumberType.UNIT
				case _ => NumberType.NONE
			}
		} else {
			NumberType.NONE
		}
	}

	def originalType:String = time.get[String,OriginalTimeTypeAnnotation](
			classOf[OriginalTimeTypeAnnotation])
	def originalValue:String = time.get[String,OriginalTimeValueAnnotation](
			classOf[OriginalTimeValueAnnotation])

	def tid:Int = index
	def words(test:Boolean):Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			//(get annotation)
			val numVal = lbl.get[Number,NumericCompositeValueAnnotation](
				classOf[NumericCompositeValueAnnotation])
			val t = numType(lbl.get[String,NumericCompositeTypeAnnotation](
				classOf[NumericCompositeTypeAnnotation]))
			val w = if(t != NumberType.NONE){ numVal.toString } else { lbl.word }
			//(get word)
			if(test) {
				U.str2wTest(w, t) 
			} else {
				U.str2w(w, t)
			}
		}.toArray
	}
	def gloss:String = {
		span.map{ (lbl:CoreLabel) => lbl.word }.mkString(" ")
		
	}
	def pos(test:Boolean):Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			if(test) U.str2posTest(lbl.tag) else U.str2pos(lbl.tag)
		}.toArray
	}
	def nums:Array[Int] = { 
		span.map{ (lbl:CoreLabel) => 
			//(get annotation)
			val numVal = lbl.get[Number,NumericCompositeValueAnnotation](
				classOf[NumericCompositeValueAnnotation])
			val numType = lbl.get[String,NumericCompositeTypeAnnotation](
				classOf[NumericCompositeTypeAnnotation])
			//(get number)
			if(numVal != null){
				numVal.intValue
			} else {
				-1
			}
		}.toArray
	}
	def gold:Temporal
		= DataLib.array2JodaTime(
			time.get[Array[String],TimeValueAnnotation](classOf[TimeValueAnnotation]))
	
	def grounding:Time = pubTime

	override def toString:String 
		= "timex["+tid+"] "+words(false).map{ U.w2str(_) }.mkString(" * ")
//		= "timex["+tid+"] "+span.map{ _.word }.mkString(" * ")
}
