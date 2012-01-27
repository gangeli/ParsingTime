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

def AUTO_MISS(id:String,sent:TimeSent,gold:Any) = 
"""
################################################################################
# Datum """+id+"""
################################################################################
slide!('Datum """+id+""" \red{(no parses)}',
	'',
	center,
	_('TimeSent: """+
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

def CRF_DATA_NOAMBIGUITY:Array[(TimeSent,Array[Int])] = {
	val strings = Array[String]("today", "week", "last week today", "last week", 
		"month", "a month", "april", "a year")
	strings.map{ (str:String) =>
		str.split(" ").map{ (w:String) =>
			(U.str2w(w),w)
		}
	}.map{ (arr:Array[(Int,String)]) =>
		val words = arr.map(_._1)
		(TimeSent(
				-1,
				words, 
				words.map{(w:Int) => U.str2pos("POS")}, 
				words.map{w => -1} ),
			words
		)
	}
}
}


