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
def fstr(str)
	str.gsub("F_RR",'$f(R,R):R$').gsub(
		"F_R",'$f(R):R$').gsub(
		"F_D",'$f(D):R$').gsub(
		"F_RD",'$f(R,D):R$')
end

################################################################################
# Title
################################################################################
slide!('"""+name+"""',
nil){ |slide| slide.label('title').signature(1).titleSpacing(u(0)) }
"""
}

def SLIDE(id:Int,correct:Boolean,
	tree:String,guess:String,gold:String,ground:String) = 
"""
################################################################################
# Datum """+id+"""
################################################################################
slide!('Datum """+id+""" ("""+
	{if(correct) """\green{correct}""" else """\red{incorrect}"""}+""")',
	'',
	center,
	Parse.new(fstr('"""+tree+"""')).constituency,
	'',
	left,
	'\blue{Guess:} """+guess+"""',
	'\green{Gold:} """+gold+"""',
	'Grounding: """+ground+"""',
nil){ |slide| slide.label('datum"""+id+"""').signature(0).titleSpacing(u(0)) }
"""

def END_PRESENTATION = 
"""
################################################################################
# Finish
################################################################################
finishPresentation
"""
}
