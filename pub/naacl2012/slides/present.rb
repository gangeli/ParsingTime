#!/usr/bin/ruby

require 'rfig/Presentation'
require 'figlib.rb'
require 'include.rb'

"""
	Point-based versus Interval-based semantics of time
	'Temporal Semantics'
	Look at TempEval2 guidelines
"""

#--Set Slide Style
slideStyle = nil
slideStyle = SlideStyle.new.border(1).borderColor(black)

#--Init Presentation
initPresentation(
	:latexHeader => IO.readlines("#{ENV['HOME']}/lib/latex/std-macros.tex") +
		IO.readlines("../macros.tex") +
		['\def\tp#1{\darkgreen{\textit{#1}}}
		\def\te#1{\darkred{\texttt{#1}}}
		\def\ground#1{\blue{\texttt{#1}}}
		'],
	:edition => 'present',
	:fontSize => 32
).slideStyle(slideStyle)

################################################################################
# TITLE
################################################################################
slide!('',
	center,
	rtable(
		image('img/logo.jpg').scale(0.25),
		_('Parsing Time').scale(2.0).color(darkred),
		ctable(
			author(_('Gabor Angeli').color(darkblue)),
			author('Christopher Manning'),
			author('Dan Jurafsky'),
		nil).cmargin(u(0.5)),
	nil).rmargin(u(0.75)).cjustify('c'),
	left,
nil){ |slide| slide.label('title').signature(31) }

################################################################################
# OUTLINE
################################################################################
slide!('Outline',
	staggeredOverlay(true,
		outline(nil),
		outline(0),
	nil),
nil){ |slide| slide.label('outline').signature(9) }

################################################################################
# EXAMPLE
################################################################################
slide!('Example',
	center,
	staggeredOverlay(true,
		example,
		example(true),
		example(true,0),
		example(true,1),
		example(true,1,true),
		example(true,1,true,true),
	nil),
	left,

	'',
	defn('Detection',
		'Finding temporal \textit{phrases} in a sentence.').level(1),
	'',
	defn('Interpretation',
		'Finding the grounded \textit{meaning} of a phrase').level(2),
	ind('But, often incomplete information').level(3),
	ind('Incorporate a \ground{reference time}').level(4),
	ind('Sneak Peek: ambiguity in \tp{next week}').level(5),
nil){ |slide| slide.label('motivation').signature(88) }



################################################################################
# FINISH
################################################################################
finishPresentation





