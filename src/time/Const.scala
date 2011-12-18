package time

object Const {
def START_PRESENTATION(name:String) = {
"""#!/usr/bin/ruby

require 'rfig/Presentation'
require "#{ENV['HOME']}/workspace/time/aux/figlib.rb"

#--Set Slide Style
slideStyle = nil
slideStyle = SlideStyle.new.border(1).borderColor(black)

#--Init Presentation
initPresentation(
	:latexHeader => IO.readlines("#{ENV['HOME']}/lib/latex/std-macros.tex"),
	:edition => 'present',
	:fontSize => 27
).slideStyle(slideStyle)

################################################################################
# UTILITIES
################################################################################
"""
}

def SLIDE(id:String,correct:Boolean,tree:String,probs:String,
		guess:String,gold:String,ground:String,score:Double) = 
"""
################################################################################
# Datum """+id+"""
################################################################################
slide!('Datum """+id+""" ("""+
	{if(correct) """{\green correct}""" else """{\red incorrect}"""}+""")',
	'',
	center,
	Parse.new('"""+tree+"""').probabilities('"""+probs+"""').constituency,
	'',
	left,
	'\blue{Guess:} """+guess+""" ["""+G.df.format(score)+"""]',
	'\green{Gold:} """+gold+"""',
	'Grounding: """+ground+"""',
nil){ |slide| slide.label('datum"""+id+"""').signature(0).titleSpacing(u(0)) }
"""

def DIFF(id:String,
		guess:String,guessProbs:String,
		gold:String,goldProbs:String) = {
"""
################################################################################
# """+id+"""
################################################################################
slide!('Datum """+id+"""',
	'',
	center,
	Parse.new(fstr('"""+guess+"""')).probabilities('"""+guessProbs+"""').constituency,
	'\blue{(first correct)}',
	Parse.new(fstr('"""+gold+"""')).probabilities('"""+goldProbs+"""').constituency,
nil){ |slide| slide.label('datum"""+id+"""').signature(0).titleSpacing(u(0)) }
"""
}

def AUTO_MISS(id:String,sent:Sentence,gold:Any) = 
"""
################################################################################
# Datum """+id+"""
################################################################################
slide!('Datum """+id+""" \red{(no parses)}',
	'',
	center,
	_('Sentence: """+
		sent.toString.replaceAll("\"","\\\\\"").replaceAll("'","\\\\'")+"""'),
	_('Gold: """+{if(gold != null) gold else "???"}+"""'),
nil){ |slide| slide.label('datum"""+id+"""').signature(0).titleSpacing(u(0)) }
"""

def END_PRESENTATION = 
"""
################################################################################
# Finish
################################################################################
finishPresentation
"""

def CRF_DATA_NOAMBIGUITY:Array[(Sentence,Array[Int])] = {
	val strings = Array[String]("today", "week", "last week today", "last week", 
		"month", "a month", "april", "a year")
	strings.map{ (str:String) =>
		str.split(" ").map{ (w:String) =>
			(U.str2w(w),w)
		}
	}.map{ (arr:Array[(Int,String)]) =>
		val words = arr.map(_._1)
		(Sentence(
				-1,
				words, 
				words.map{(w:Int) => U.str2pos("POS")}, 
				words.map{w => -1} ),
			words
		)
	}
}

import CKYParser._
lazy val goldTag:(Sentence,(CkyRule,Int,Double)=>Boolean)=>Array[Int] = {
	import scala.io.Source.fromFile
	import java.io.File
	import scala.collection.immutable.Map
	val Sent = """([0-9]+)\s*::\s*(.*?)\s*::\s*(.*?)""".r
	val Fail = """\s+[Ff][Aa][Ii][Ll]\s*""".r
	val Tags = """\s+(.+)""".r
	val Tag =  """[^\s]+""".r
	val Whitespace = """\s*""".r
	val tagMap:Map[Int,Array[Int]] = fromFile(new File(O.goldTagFile))
			.getLines.foldLeft(
			(-1,List[(Int,Array[Int])]())){ 
			case ((sid:Int,soFar:List[(Int,Array[Int])]),line:String) =>
		line match {
			case Sent(newSid,gloss,value) => (newSid.toInt,soFar)
			case Fail(_*) => {
				assert(sid >= 0, "No SID for [failed] line " + line)
				(-1,(sid,Array[Int]()) :: soFar)
			}
			case Tags(tags) => {
				assert(sid >= 0, "No SID for line " + line)
				(-1, 
					(sid,Tag.findAllIn(tags).map( Grammar.str2rid(_) ).toArray) :: soFar)
			}
			case Whitespace(_*) => (sid,soFar)
			case _ => throw new IllegalArgumentException("Unknown line: '"+line+"'")
		}
	}._2.toMap
	//--Function
	(sent:Sentence,y:(CkyRule,Int,Double)=>Boolean) => {
		val tags = tagMap(sent.id)
		if(tagMap(sent.id) == null){ 
			throw new IllegalStateException("No saved tags for sid " + sent.id)
		}
		assert(tags.length == 0 || sent.length == tags.length,
			"Bad tag count for " + sent.id + ": " + sent + " sent: " 
				+ sent.length + " vs tags: " + tags.length)
		if(tags.length == 0){
			throw new IllegalArgumentException("No tags given")
//			(0 until sent.length).toArray.map{ (i:Int) =>
//				y(CKY_LEX(rid2lexI(Grammar.NIL_RID)),i,0.0)
//				1
//			}
		} else {
			tags.zipWithIndex.map{ case (tag:Int,i:Int) =>
				y(CKY_LEX(rid2lexI(tag)),i,0.0)
				1
			}
		}
	}
}








}


