package time

object Const {
def START_PRESENTATION = 
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
	str.gsub("F_RR",'"$(R,R) \\\\\\\\rightarrow R$"').gsub(
		"F_R",'"$(R) \\\\\\\\rightarrow R$"').gsub(
		"F_D",'"$(D) \\\\\\\\rightarrow R$"').gsub(
		"F_RD",'"$(R,D) \\\\\\\\rightarrow R$"')
end

################################################################################
# Title
################################################################################
slide!('Parses Summary',
nil){ |slide| slide.label('title').signature(1).titleSpacing(u(0)) }
"""

def SLIDE(id:Int,tree:String,guess:String,gold:String) = 
"""
################################################################################
# Datum """+id+"""
################################################################################
slide!('Datum """+id+"""',
	'',
	center,
	Parse.new(fstr("""+"\"\"\""+tree+"\"\"\""+""")).constituency,
	'',
	left,
	'\blue{Guess:} """+guess+"""',
	'\green{Gold:} """+gold+"""',
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
