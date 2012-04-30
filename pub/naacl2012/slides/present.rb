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
nil){ |slide| slide.label('intro_title').signature(31) }

################################################################################
# OUTLINE
################################################################################
slide!('Outline',
	staggeredOverlay(true,
		outline(nil),
		outline(0),
	nil),
nil){ |slide| slide.label('intro_outline').signature(9) }

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
nil){ |slide| slide.label('motivation_example').signature(91) }

################################################################################
# MOTIVATION
################################################################################
slide!('Motivation',
	#(parsing of meaning)
	h1('Deep parsing of meaning'),
	ind('Complete understanding of interpreted expressions'),
	'',
	pause,
	ind('Handle \textit{syntactic} ambiguity'),
	ind(ind('\tp{[last Friday] [the \th{13}]} or \tp{[last] [Friday the \th{13}]}')),
	'',
	pause,
	ind('Handle \textit{pragmatic} ambiguity'),
	ind(ind('\tp{last Sunday} as \te{June 3, 2012} or \te{May 27, 2012}')),
	'',
	pause,
	#(temporal info useful)
	h1('Temporal information useful'),
	ind('Information (e.g., relation) extraction'),
	'',
	pause,
	ind('Finding times/appointments in correspondences'),
	'',
	pause,
	ind('Ranking urgency'),

nil){ |slide| slide.label('motivation_motiv').signature(41) }

################################################################################
# RELATED
################################################################################
slide!('Related Work',
	_('\darkred{\textbf{[Semantic] \darkblue{Parsing}}}'),
	staggeredOverlay(true,
		relatedParsing(2),
		relatedParsing(0),
		relatedParsing(1),
		relatedParsing(2),
	nil),
	'','',
	_('\textbf{\darkblue{Time} \darkred{[Interpretation]}}'),
	staggeredOverlay(true,
		relatedSemantics(0),
		relatedSemantics(1),
	nil),
nil){ |slide| slide.label('motivation_related').signature(8) }

################################################################################
# SYSTEM
################################################################################
slide!('System',
	'',
	center,
	staggeredOverlay(true,
		sys(0),
		sys(1),
		sys(1,0),
		sys(1,1),
		sys(1,1,true),
		sys(1,1,true,true),
	nil),
	left,
nil){ |slide| slide.label('motivation_system').signature(14) }

################################################################################
# PARSE -- VALUES
################################################################################
slide!('Latent Parse',
	#(main point)
	h1('Expressions parse compositionally'),
	'','',
	pause,
	#(parse)
	center,
	phrase('last 2 days'),
	pause,
	'','',
	staggeredOverlay(true,
		last2days(:days=>1),
		last2days(:days=>1, :two=>1),
		last2days(:days=>1, :two=>1, :twodays=>1),
		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1),
		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1),
	nil),
	pause,
	'',
	'',
	#(value)
	ctable(ground('[June 5, 2012]'), '$\rightarrow$', time('June 3, 2012 \textrm{--} June 5, 2012')),
	left,
nil){ |slide| slide.label('motivation_parse').signature(60) }

################################################################################
# PARSE -- TYPES
################################################################################
slide!('Latent Parse',
	#(intro)
	h1('Nonterminals become very sparse'),
	pause,
	ind(ctable('Consider: ',phrase('last 3 days'),', ',phrase('last 2 months'),', etc.')),
	pause,
	ind('\textbf{Solution:} Group nonterminals based on \textit{types}'),
	pause,
	'','',
	#(type parse)
	center,
	staggeredOverlay(true,
		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1),
		last2days(:days=>0, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1),
		last2days(:days=>0, :two=>0, :twodays=>1, :last=>1, :lasttwodays=>1),
		last2days(:days=>0, :two=>0, :twodays=>0, :last=>1, :lasttwodays=>1),
		last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>1),
		last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>0),
		last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>0, :change=>true),
	nil),
	left,
	'','',
	pause,
	'What are these nonterminals, and how do they combine?',
nil){ |slide| slide.label('motivation_parse2').signature(10) }

################################################################################
# REPRESENTATION
################################################################################
slide!('Outline',
	outline(1),
nil){ |slide| slide.label('representation_outline').signature(12) }

################################################################################
# NONTERMINAL TYPES
################################################################################
slide!('Grammar Of Time',
	h1(mcal('Range')),
	'',
	h1(calendar('Sequence')),
	'',
	h1(hourglass('Duration')),
	'',
	pause,
	h1('Functions'),
	'',
	h1('Number'),
	'',
	h1('Nil (no temporal meaning)'),
nil){ |slide| slide.label('representation_type_overview').signature(12) }

################################################################################
# RANGE
################################################################################
slide!('Grammar Of Time',
	h1(mcal('Range')),
	ind('A period between two dates (or times)'),
	'',
	pause,
	ind('8 lexical values: \te{yesterday}, \te{tomorrow}, \te{today}, \te{reference},'),
	ind(ind('\te{past}, \te{future}, \te{year$(n)$}, \te{century$(n)$}')),
	'',
	pause,
	ind('Also, e.g., \tp{June 5, 2012}, \tp{November 1987}, \tp{day before yesterday}'),
	'',
	pause,
	ind('Interval-based theory of time: instants are ranges'),
	ind(ind('\te{Today} and \te{Reference} both ranges, but former has duration')),
nil){ |slide| slide.label('representation_range').signature(3) }

################################################################################
# SEQUENCE
################################################################################
slide!('Grammar Of Time',
	h1Grey(mcal('Range')),
	h1(calendar('Sequence')),
	ind('A sequence of Ranges (not necessarily at regular intervals)'),
	'',
	pause,
	ind('34 lexical values, e.g.: \te{Friday}, \te{Saturday}, \te{January},'),
	ind(ind('\te{DayOfMonth}, \te{DayOfWeek}, \te{WeekOfYear},')),
	ind(ind('\te{EveryDay}, \te{EveryWeek}, \te{year$(n)$}, \te{century$(n)$}')),
	'',
	pause,
	ind('Also, e.g., \tp{June 5}, \tp{next Saturday}, \tp{third quarter}'),
	'',
	pause,
	ind('\red{Still stuck:} which element are we referring to?'),
nil){ |slide| slide.label('representation_sequence').signature(7) }

################################################################################
# SEQUENCE AMBIGUITY
################################################################################
slide!('Grammar Of Time',
	h1Grey(mcal('Range')),
	h1(calendar('Sequence')),
	#(answer)
	ind('\green{Answer:} We\'re referring to all of them! (Kind of$\dots$)'),
	#(distribution)
	'',
	pause,
	ind('Today is \ground{June 5, 2012}, what is \tp{next Saturday}?'),
	'',
	pause,
	center,
	staggeredOverlay(true,
		nextFridayDistribution([3],[3]),
		nextFridayDistribution([2,3],[2]),
		nextFridayDistribution([1,2,3,4],[1,4]),
		nextFridayDistribution([1,2,3,4],[],true),
	nil),
	left,
	#(characterize gaussian)
	'',
	pause,
	ind('Construct a Gaussian over possible groundings'),
	pause,
	ind('Learn Gaussian parameters $\mu,\sigma$'),


nil){ |slide| slide.label('representation_sequence_ambiguity').signature(19) }

################################################################################
# DURATION
################################################################################
slide!('Grammar Of Time',
	h1Grey(mcal('Range')),
	h1Grey(calendar('Sequence')),
	h1(hourglass('Duration')),
	ind('A period of time'),
	'',
	pause,
	ind('10 lexical values: \te{second}, \te{minute}, \te{hour}, \te{day}, \te{week}, \te{month},'),
	ind(ind('\te{quarter}, \te{year}, \te{decade}, \te{century}')),
	'',
	pause,
	ind('Also, e.g., \tp{2 weeks}, \tp{10 years}'),
	'',
	pause,
	ind('10 more \textit{approximate} durations, e.g., for \tp{some weeks}'),

nil){ |slide| slide.label('representation_duration').signature(10) }



################################################################################
# FINISH
################################################################################
finishPresentation





