#!/usr/bin/ruby
# encoding: utf-8

require 'rfig/Presentation'
require './figlib.rb'
require './include.rb'

#--Set Slide Style
slideStylePlain = SlideStyle.new.
	border(1).
	borderColor(black).
	titleSpacing(u(0.2))
slideStyle = SlideStyle.new.
	border(1).
	borderColor(black).
	titleSpacing(u(0.2)).
	rightHeader(image('img/google_small.jpg').scale(0.40))

#--Init Presentation
initPresentation(
	:latexHeader => IO.readlines("#{ENV['HOME']}/lib/latex/std-macros.tex") +
		IO.readlines("../macros.tex") +
		['\def\tp#1{\darkgreen{\textit{#1}}}
		\def\te#1{\darkred{\texttt{#1}}}
		\def\ground#1{\blue{\texttt{#1}}}
    \usepackage{CJKutf8}
    \newcommand{\zht}[1]{\begin{CJK}{UTF8}{bsmi}#1\end{CJK}}
    \newcommand{\zhs}[1]{\begin{CJK}{UTF8}{gbsn}#1\end{CJK}}
    \newcommand{\zh}[4]{\zht{#1}/\zhs{#2} (\emph{#3}) ``#4''}
		'],
	:edition => 'present',
	:fontSize => 32
)

################################################################################
# TITLE
################################################################################
slide!('',
	center,
	rtable(
		_('Multilingual Temporal Parsing').scale(2.0).color(darkred),
		ctable(
      author(_('Gabor Angeli').color(darkblue)),
	    author('Jakob Uszkoreit'),
    nil).cmargin(u(1.0)),
		image('img/google.jpg').scale(0.05),
	nil).rmargin(u(1.75)).cjustify('c'),
	left,
nil){ |slide| slide.label('intro_title').signature(69) }

################################################################################
# OUTLINE
################################################################################
#slide!('Outline',
#	staggeredOverlay(true,
#		outline(nil),
#		outline(0),
#	nil),
#nil){ |slide| slide.label('intro_outline').slideStyle(slideStyle).signature(13) }

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
nil){ |slide| slide.label('motivation_example').slideStyle(slideStyle).slideStyle(slideStyle).signature(194) }


################################################################################
# EXAMPLE -- PRACTICAL
################################################################################
slide!('Time In Information Extraction',
	h1('News'),
  ind(w('[The falcon] was found injured \textbf{last Thursday} in $\dots$')),
  ind(ind(w('$\dots$ she died \textbf{early Saturday morning}'))),
#	ind(w('Beginning more than \\textbf{seven hours earlier}, the space station\'s')),
#	ind(ind(w('robotic arm detached the 14-foot long Dragon [spacecraft]'))),
	'',
	ind(w('Benjamin Franklin Federal Savings and Loan Association said it')),
	ind(ind(w('plans to restructure in the wake of a \textbf{third-quarter} loss'))),
	'','',
	pause,

	h1('Communication'),
	ind(w('Actually I am on vacation the \textbf{last three weeks of November}')),
	'',
	ind(w('I have some time available at the \textbf{end of next week}')),
nil){ |slide| slide.label('motivation_example_practical').slideStyle(slideStyle).signature(14) }

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
nil){ |slide| slide.label('motivation_system').slideStyle(slideStyle).signature(39).footnote(
	'\darkblue{Angeli \textit{et al.} 2012}'
)}

#################################################################################
## NONTERMINAL TYPES
#################################################################################
slide!('Elements of Latent Parse',
  table(
    [h1(range), phrase("the week of August 5, 2013")],
    [h1(sequence), phrase("week")],
    [h1(duration), phrase("a week")],
    ['Nil',        phrase("this")],
    ['Functions',  ctable(phrase('last'), '', '[', time('takeLeft$(-)$'), ']').cmargin(u(0.15))],
  ).rmargin(u(0.5)).cmargin(u(1.0)),
nil){ |slide| slide.label('representation_type_overview').slideStyle(slideStyle).signature(22) }

################################################################################
# MOTIVATION -- PREVIOUS
################################################################################
slide!('Prior Work',
	h1('Hand coded rules'),
	ind('\textbf{Rigid}'),
  staggeredOverlay(true,
	  ind(ind('Syntax: \tp{last Friday the \th{12}}')),
	  ind(ind('Syntax: \tp{last Friday the \th{13}}')),
  nil),
  pause,
  staggeredOverlay(true,
	  ind(ind('Domain pragmatics: \tp{A year ago}')),
	  ind(ind('Domain pragmatics: \tp{Sales are down from \textbf{a year ago}}')),
	  ind(ind('Domain pragmatics: \tp{Remember? We got married \textbf{a year ago}}')),
  nil),
	'',
	pause,
	ind('\textbf{Rule engineering challenge}'),
	ind(ind('Always more rules: \tp{2 days \textbf{prior}}, \tp{the \textbf{previous} 2 days}')),
	ind(ind('\verb~/the/ /past|last/ (?: ($NUM) /to|-/ )? ($NUM)? ($TEUNITS)~').scale(0.75)),
	ind(ind('\verb~/the/ /next|following/ (?: ($NUM) /to|-/ )? ($NUM)? ($TEUNITS)~').scale(0.75)),
	ind(ind('$\dots$')),

  pause,
  ind(h1('New set of rules for each language')),
#  h1(ctable(new, 'Adapt to multiple languages')),
#  ind(ind('Annotating data easier than adapting rules')),
nil){ |slide| slide.label('motivation_motiv').slideStyle(slideStyle).signature(44).footnote(
	'\darkblue{Mani \& Wilson (2000); Str\"{o}tgen and Gertz (2010); Chang and Manning (2012)}'
)}

################################################################################
# MOTIVATION -- US
################################################################################
slide!('Motivation',
#	h1('Our Model'),
#	ind('\textbf{Gives confidence}'),
#	pause,
#	ind('\textbf{Universal temporal representation}'),
#	pause,
#	ind('\textbf{Domain flexible}'),
#	ind(ind('Emulates pragmatics of training domain')),
#	ind(ind('What is \tp{a year ago}?')),
#	pause,

	h1('\textbf{Language independent}'),
	ind('Arbitrary training data: \tp{\textbf{Last} Sunday} / \tp{domingo \textbf{pasado}}'),
  pause,
  staggeredOverlay(true,
    rtable(
      ind('\textbf{Hard}:'),
      ind(ind((ctable(phrase('pasado'), '$\rightarrow$', value('\texttt{last}$($--$)$'))))),
      ind(ind((ctable(phrase('domingo'), '$\rightarrow$', sunday.scale(0.75))))),
	    ind(ind('\verb~($DOW) /pasad[oa]/~')),

    nil),
    rtable(
      ind('\textbf{Easier}: annotate training data for \tp{domingo pasado}'),
#      ind(ind(ind(
#        ctable('Ask native speaker `when was ', phrase('domingo pasado'), '?\'')))),
    nil),
  nil),
nil){ |slide| slide.label('motivation_motiv2').slideStyle(slideStyle).signature(36) }

################################################################################
# MOTIVATION -- RULES
################################################################################
slide!('Motivation',
#	h1('Our Model'),
#	ind('\textbf{Gives confidence}'),
#	pause,
#	ind('\textbf{Universal temporal representation}'),
#	pause,
#	ind('\textbf{Domain flexible}'),
#	ind(ind('Emulates pragmatics of training domain')),
#	ind(ind('What is \tp{a year ago}?')),
#	pause,

	h1('\textbf{Language independent}'),
	ind('Arbitrary training data: \tp{\textbf{Last} Sunday} / \tp{domingo \textbf{pasado}}'),
  rtable(
    ind('\textbf{Easier}: annotate training data for \tp{domingo pasado}'),
#    ind(ind(ind(
#      ctable('Ask native speaker `when was ', phrase('domingo pasado'), '?\'')))),
  nil),
	
#  h1('Define a single grammar'),
	ind('$\sim 100$ grammar rules total'),
	ind('Same learning algorithm, hyperparameters'),
  '',
  pause,
  h1('HeidelTime'),
	ind('English: $\sim 2000$ rules (185 combination rules)'),
  pause,
	ind('Spanish: $\sim 1200$ rules (167 combination rules)'),
	ind('Italian: $\sim 1600$ rules (156 combination rules)'),
  ind('$\dots$'),
  pause,
  h1('SUTime'),
	ind('English: $\sim 900$ rules'),
nil){ |slide| slide.label('motivation_numrules').slideStyle(slideStyle).signature(39) }


################################################################################
# MOTIVATION
################################################################################
#slide!('Motivation',
#	#(parsing of meaning)
#	h1('Deep parsing of meaning'),
#	ind('Complete understanding of interpreted expressions'),
#	'',
#	pause,
#	ind('Handle \textit{syntactic} ambiguity'),
#	ind(ind('\tp{[last Friday] [the \th{13}]} or \tp{[last] [Friday the \th{13}]}')),
#	'',
#	pause,
#	ind('Handle \textit{pragmatic} ambiguity'),
#	ind(ind('\tp{last Sunday} as \te{June 3, 2012} or \te{May 27, 2012}')),
#	'',
#	pause,
#	#(temporal info useful)
#	h1('Temporal information useful'),
#	ind('Information (e.g., relation) extraction'),
#	'',
#	pause,
#	ind('Finding times/appointments in correspondences'),
#	'',
#	pause,
#	ind('Ranking urgency'),
#
#nil){ |slide| slide.label('motivation_motiv').slideStyle(slideStyle).signature(46) }

################################################################################
# RELATED
################################################################################
#slide!('Related Work',
#	_('\darkred{\textbf{[Semantic] \darkblue{Parsing}}}'),
#	staggeredOverlay(true,
#		relatedParsing(2),
#		relatedParsing(0),
#		relatedParsing(1),
#		relatedParsing(2),
#	nil),
#	'','',
#	_('\textbf{\darkblue{Time} \darkred{[Interpretation]}}'),
#	staggeredOverlay(true,
#		relatedSemantics(0),
#		relatedSemantics(1),
#	nil),
#nil){ |slide| slide.label('motivation_related').slideStyle(slideStyle).signature(9) }

################################################################################
# MOTIVATION -- COMPARISON
################################################################################
#slide!('Comparison To Lambda Calculus',
#	center,
#	plet(:startlevel, tstartlevel),
#	staggeredOverlay(true,
#		_('Parallels to semantic parsing').color(darkred),
#		pause,
#		_('\textbf{Option 1}: bootstrap from parse').color(darkred),
#		pause,
#		_('\textbf{Option 2}: bootstrap from grounded interpretation').color(darkred),
#	nil).pivot(0,0),
#	'',
#	plevel(:startlevel),
#	staggeredOverlay(true,
#		comparison(0,-1,false,false),
#		comparison(0, 0,false,false),
#		comparison(0, 0,true ,false),
#		comparison(0, 0,true ,true),
##		comparison(1, 1,false,false),
#		comparison(2, 2,false,false),
#		pause,
#	nil),
#	left,
#nil){ |slide| slide.label('motivation_compare').slideStyle(slideStyle).signature(100).footnote(
#	_('\darkblue{Zettlemoyer \& Collins 2005/2007; Liang et al. 2011}')
#)}

################################################################################
# PARSE -- VALUES
################################################################################
slide!('Language Independent',
	#(main point)
	h1('Latent semantic representation'),
	h1('Many languages share representation').color(nocolor),
#	pause,
	#(parse)
	center,
	phrase('last 2 days'),
#	pause,
	'','',
	staggeredOverlay(true,
#		last2days(:days=>1),
#		last2days(:days=>1, :two=>1),
#		last2days(:days=>1, :two=>1, :twodays=>1),
#		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1),
		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1),
	nil),
#	pause,
	'',
	'',
	#(value)
	ctable('[',ground(Time.mktime(2013,8,5)),']', '$\rightarrow$', time([Time.mktime(2013,8,3),'$-$',Time.mktime(2013,8,5)])),
	left,
nil){ |slide| slide.label('motivation_parse').slideStyle(slideStyle).signature(72) }

################################################################################
# PARSE -- Multilingual
################################################################################
slide!('Language Independent',
	#(main point)
	h1('Latent semantic representation'),
	h1('Many languages share representation'),
	#(parse)
	center,
  overlay(
  	phrase('\\\'ultimos dos dias').level(0,1),
  	phrase('ultimi due giorni').level(1,2),
    phrase('el\\H{o}z\\H{o} k\\\'et nap').level(2,3),
    phrase('\zht{最後兩天}').level(3,4),
    phrase('deux derniers jours').level(4),
  nil),
	'','',
  overlay(
	  last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1,
      :lang => ['\\\'ultimos', 'dos', 'dias']).level(0,1),
	  last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1,
      :lang => ['ultimi', 'due', 'giorni']).level(1,2),
	  last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1,
      :lang => ['el\\H{o}z\\H{o}', 'k\\\'et', 'nap']).level(2,3),
	  last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1,
      :lang => ['\zht{最後}', '\zht{兩}', '\zht{天}']).level(3,4),
	  last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1,
      :lang => ['jours', 'deux', 'derniers'], :switchOrder=>true).level(4),
  nil),
	'',
	'',
	#(value)
	ctable('[',ground(Time.mktime(2013,8,5)),']', '$\rightarrow$', time([Time.mktime(2013,8,3),'$-$',Time.mktime(2013,8,5)])),
	left,
nil){ |slide| slide.label('motivation_parse_multilingual').slideStyle(slideStyle).signature(20) }

################################################################################
# LANGUAGE INDEPENDENT -- COMPARISON
################################################################################
#slide!('Language Independent',
#	h1('Define a single grammar'),
#	ind('$\sim 100$ grammar rules total'),
#	ind('Same learning algorithm, hyperparameters'),
#  '','',
#  pause,
#  h1('HeidelTime'),
#	ind('English: $\sim 2000$ rules (185 combination rules)'),
#  pause,
#	ind('Spanish: $\sim 1200$ rules (167 combination rules)'),
#	ind('Italian: $\sim 1600$ rules (156 combination rules)'),
#  ind('$\dots$'),
#  '','',
#  pause,
#  h1('SUTime'),
#	ind('English: $\sim 900$ rules'),
#nil){ |slide| slide.label('motivation_compare').slideStyle(slideStyle).signature(5).footnote(
#	'\darkblue{Str\"{o}tgen and Gertz (2010); Chang and Manning (2012)}'
#)}
################################################################################
# PARSE -- TYPES
################################################################################
slide!('Parsing',
  h1('Parsing temporal phrases'),
  ind('Explained in, but not specific to English'),
  ind('Na\\"ively, domain of nonterminals is large').level(1),
  '',
  center,
  staggeredOverlay(true,
	  last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1,
      :lang => ['last', 'two', 'days']),
	  last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1,
      :lang => ['last', 'two', 'days'], :change => ['three', 'days']),
	  last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1,
      :lang => ['last', 'two', 'days'], :change => ['four', 'days']),
  nil),
  left,
nil){ |slide| slide.label('learn_segway').slideStyle(slideStyle).signature(19) }


################################################################################
# PARSE -- TYPES
################################################################################
slide!('Parsing',
	#(intro)
	h1('Domain of nonterminals is large'),
#	pause,
	ind(ctable('Consider: ',phrase('last 7 days'),', ',phrase('last 3 months'),', etc.')),
#	pause,
	ind('\textbf{Generative Grammar:} Group nonterminals based on \textit{types}'),
  pause,
	'',
	#(type parse)
  staggeredOverlay(true,

  	staggeredOverlay(true,
  		ind(ind(ind(ind(last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1))))),
  #		ind(ind(ind(ind(last2days(:days=>0, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1))))),
  #		ind(ind(ind(ind(last2days(:days=>0, :two=>0, :twodays=>1, :last=>1, :lasttwodays=>1))))),
  #		ind(ind(ind(ind(last2days(:days=>0, :two=>0, :twodays=>0, :last=>1, :lasttwodays=>1))))),
  #		ind(ind(ind(ind(last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>1))))),
  		ind(ind(ind(ind(last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>0))))),
  		ind(ind(ind(ind(last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>0, :change=>[3, 'months']))))),
    nil),

    ind(ind(ind(ind(table([intersectImplausible(false, false), intersectImplausible(false, true)],
          [darrow, darrow].color(nocolor),
          [intersectImplausible(true, false), intersectImplausible(true, true)].color(nocolor)
          ).cmargin(u(2.5)).cjustify('c').rmargin(u(0.25)))))),
    ind(ind(ind(ind(table([intersectImplausible(false, false), intersectImplausible(false, true)],
          [darrow, darrow],
          [intersectImplausible(true, false), intersectImplausible(true, true)]
          ).cmargin(u(2.5)).cjustify('c').rmargin(u(0.25)))))),

    rtable('$~$',h1('Discriminative model'),
      ind(table(
        ['Coarse-grained features over types:', range],
        ['Fine grained features over values:', theyear('2013')],
      nil).cmargin(u(0.25))),
      '$~$',
	    ctable(h1('Adapt semantic parser'), '(e.g., Liang \textit{et al.} 2011)').cmargin(u(0.2)),
    nil),
  nil),
nil){ |slide| slide.label('motivation_parse2').slideStyle(slideStyle).signature(69).footnote(
	'\darkblue{Angeli \textit{et al.} 2012}'
)}

################################################################################
# REPRESENTATION
################################################################################
#slide!('Outline',
#	outline(1),
#nil){ |slide| slide.label('representation_outline').slideStyle(slideStyle).signature(13) }

#################################################################################
## NONTERMINAL TYPES
#################################################################################
#slide!('Grammar Of Time',
#	h1(range),
#	'',
#	h1(sequence),
#	'',
#	h1(duration),
#	'',
#	pause,
#	h1(time('Functions')),
#	'',
#	h1(time('Number')),
#	'',
#	h1(time('Nil (no temporal meaning)')),
#nil){ |slide| slide.label('representation_type_overview').slideStyle(slideStyle).signature(12) }
#
#################################################################################
## RANGE
#################################################################################
#slide!('Grammar Of Time',
#	h1(range),
#	ind('A period between two dates (or times)'),
##	'',
##	pause,
##	ind('8 lexical categories: \te{yesterday}, \te{tomorrow}, \te{today}, \te{reference},'),
##	ind(ind('\te{past}, \te{future}, \te{year$(n)$}, \te{century$(n)$}')),
#	'',
#	ind('\tp{Today}, \tp{June 5 2012}, \tp{day before yesterday}'),
##	'',
##	pause,
##	ind('Interval-based theory of time: instants are ranges'),
##	ind(ind('\te{Today} and \te{Reference} both ranges, but former has duration')),
#nil){ |slide| slide.label('representation_range').slideStyle(slideStyle).signature(6) }
#
#################################################################################
## SEQUENCE
#################################################################################
#slide!('Grammar Of Time',
#	h1Grey(range),
#	h1(sequence),
#	ind('A sequence of Ranges (not necessarily at regular intervals)'),
##	'',
##	pause,
##	ind('44 lexical categories, e.g.:'),
##	ind(ind('\te{DayOfMonth}, \te{DayOfWeek}, \te{WeekOfYear},')),
##	ind(ind('\textit{shorthand}: \te{Friday}, \te{Saturday}, \te{January},')),
##	ind(ind('\textit{dense}: \te{EveryDay}, \te{EveryWeek},')),
#	'',
#	ind('\tp{June 5}, \tp{last Sunday}, \tp{third quarter}'),
##	'',
##	pause,
##	ind('\red{Still stuck:} which element are we referring to?'),
##	pause,
##	#(answer)
##	'',
##	ind('\green{Answer:} We\'re referring to all of them! (Kind of$\dots$)'),
#nil){ |slide| slide.label('representation_sequence').slideStyle(slideStyle).signature(13) }

################################################################################
# SEQUENCE AMBIGUITY
################################################################################
#slide!('Grammar Of Time',
#	h1Grey(range),
#	h1(sequence),
#	#(distribution)
#	'',
#	ind(ctable('Today is',ground(Time.mktime(2012,6,5)),', what is \tp{last Sunday}?')),
#	'',
#	pause,
#	center,
#	staggeredOverlay(true,
#		nextFridayDistribution([3],[3]),
#		nextFridayDistribution([2,3],[2]),
#		nextFridayDistribution([1,2,3,4],[1,4]),
#		nextFridayDistribution([1,2,3,4],[],true),
#	nil),
#	left,
#	#(characterize Gaussian)
#	'',
#	ind(h1('Construct a distribution over possible groundings')),
#	pause,
#	ind(ind('Appealing to model as a Gaussian')),
#	pause,
#	ind(ind('Learn Gaussian parameters $\mu,\sigma$')),
#nil){ |slide| slide.label('representation_sequence_ambiguity').slideStyle(slideStyle).signature(28) }
#
#################################################################################
## DURATION
#################################################################################
#slide!('Grammar Of Time',
#	h1Grey(range),
#	h1Grey(sequence),
#	h1(duration),
#	ind('A period of time: \tp{day}, \tp{2 weeks}, \tp{10 years}'),
##	'',
##	pause,
##	ind('10 lexical categories: \te{second}, \te{minute}, \te{hour}, \te{day}, \te{week}, \te{month},'),
##	ind(ind('\te{quarter}, \te{year}, \te{decade}, \te{century}')),
#nil){ |slide| slide.label('representation_duration').slideStyle(slideStyle).signature(13) }
#
#################################################################################
## FUNCTION
#################################################################################
#def interval(greyed=false, doublesize=false)
#  _(doublesize ? '\texttt{[$~~~~~~~~~~~~$)}' : '\texttt{[$~~~~~~$)}').color(greyed ? grey : darkred).bold
#end
#slide!('Grammar Of Time',
#	h1Grey(range),
#	h1Grey(sequence),
#	h1Grey(duration),
#	h1(time('Functions')),
#	ind('General sequence and interval operations'),
#	'',
#	pause,
#  staggeredOverlay(true,
#    ind(ctable(interval)).scale(1.5),
#    ind(overlay(ctable(interval(true)),
#                ctable('', interval).cmargin(u(1.0)),
#                ctable('\textbf{$~~\rightarrow$}'))).scale(1.5),
#    ind(overlay(ctable('', '\textbf{$~~\rightarrow$}').cmargin(u(2.5)),
#                ctable('', interval(true)).cmargin(u(1.0)),
#                ctable('', interval(false, true)).cmargin(u(1.0)))).scale(1.5),
#    ind(overlay(ctable('', interval(true, true)).cmargin(u(1.0)),
#                ctable(interval(false, true).color(white), interval).cmargin(u(1.0)))).scale(1.5),
#    ind('Move a sequence, multiply a duration, etc.'),
#  nil),
#  center,
#  '','',
#  staggeredOverlay(true,
#    rtable(phrase('a year \textbf{from} today'),
#           '...or, \tp{\textbf{next} year}?').level(2).rmargin(u(0.5)),
#    rtable(phrase('Monday \textbf{through} Friday')).level(3),
#    rtable(phrase('The day \textbf{after} tomorrow')).level(4),
#    rtable(phrase('\textbf{next} year'),
#           phrase('5 years'),
#           phrase('$\dots$')).level(5),
#  nil),
#nil){ |slide| slide.label('representation_function').slideStyle(slideStyle).signature(47) }
#
#################################################################################
## NUMBER
#################################################################################
#slide!('Grammar Of Time',
#	h1Grey(range),
#	h1Grey(sequence),
#	h1Grey(duration),
#	h1Grey(time('Functions')),
#	h1(time('Number')),
#	ind('A number, characterized by its ordinality and magnitude'),
#nil){ |slide| slide.label('representation_number').slideStyle(slideStyle).signature(3) }
#
#################################################################################
## NIL
#################################################################################
#slide!('Grammar Of Time',
#	h1Grey(range),
#	h1Grey(sequence),
#	h1Grey(duration),
#	h1Grey(time('Functions')),
#	h1Grey(time('Number')),
#	h1(time('Nil')),
#	ind('A word without direct temporal meaning'),
#	ind(ind(phrase('a week'))),
#	ind(ind(phrase('the week'))),
#nil){ |slide| slide.label('representation_nil').slideStyle(slideStyle).signature(6) }
#
#################################################################################
## COMBINATION
#################################################################################
#slide!('Grammar Of Time',
#	h1Grey(range),
#	h1Grey(sequence),
#	h1Grey(duration),
#	h1Grey(time('Functions')),
#	h1Grey(time('Number')),
#	h1Grey(time('Nil')),
#	h1('Combination Rules'),
#	ind('Predefined combination rules'),
#	ind('Type checking function application'),
#nil){ |slide| slide.label('representation_combination').slideStyle(slideStyle).signature(2) }

################################################################################
# FEATURES
################################################################################
slide!('Features',
  #(tree)
  center,
  overlay(
    fridayThisWeek.level(0,1), 
    fridayThisWeek(:friday => true).level(1,2), 
    fridayThisWeek(:friday => true, :ofthis => true).level(2,4),
    fridayThisWeek(:friday => true, :ofthis => true, :week => true).level(4,5), 
    fridayThisWeek(:friday => true, :ofthis => true, :week => true, :ofthisweek => true).level(5,9), 
    fridayThisWeek(:friday => true, :ofthis => true, :week => true, :ofthisweek => true, :root => true).level(9), 
  nil),
  left,
  '','',

  #(features)
  ind(overlay(
    table(
      ['\texttt{lex}', feat(friday, phrase('Friday'))],
      ['', '\textit{Parallel generative grammar}'],
      ['\texttt{bracket}', feat('')].color(nocolor),
    nil).cmargin(u(1.0)).level(1,2),
    table(
      ['\texttt{lex}', feat('\textsc{Nil}', phrase('of this'))],
      ['', '\textit{Parallel generative grammar}'],
      ['\texttt{bracket}', feat('')].color(nocolor),
    nil).cmargin(u(1.0)).level(2,3),
    table(
      ['\texttt{lex}', feat('\textsc{Nil}', phrase('of this'))],
      ['\texttt{lex}', feat('\textsc{Nil}', phrase('of'))],
      ['\texttt{lex}', feat('\textsc{Nil}', phrase('this'))],
      ['\texttt{bracket}', feat('')].color(nocolor),
    nil).cmargin(u(1.0)).level(3,4),
#    table(
#      ['\texttt{lex}', feat('\textsc{Nil}', phrase('of this'))],
#      ['\texttt{lex}', feat('\textsc{Nil}', phrase('of'))],
#      ['\texttt{lex}', feat('\textsc{Nil}', phrase('this'))],
#      ['\texttt{bias}', feat('\textsc{Nil\_Bias}')],
#      ['\texttt{bracket}', feat('')].color(nocolor),
#    nil).cmargin(u(1.0)).level(4,5),
    table(
      ['\texttt{lex}', feat(everyweek, phrase('week'))],
      ['\texttt{bracket}', feat('')].color(nocolor),
    nil).cmargin(u(1.0)).level(4,5),
    table(
      ['\texttt{bracket}', feat('\textsc{Nil}', '\textsc{Sequence}')],
      ['', '\textit{Parallel generative grammar}'],
      ['\texttt{bracket}', feat('')].color(nocolor),
    nil).cmargin(u(1.0)).level(5,6),
    table(
      ['\texttt{bracket}', feat('\textsc{Nil}', '\textsc{Sequence}')],
      ['\texttt{bracket}', feat('\textsc{Nil}', everyweek)],
    nil).cmargin(u(1.0)).level(6,7),
    table(
      ['\texttt{bracket}', feat('\textsc{Nil}', '\textsc{Sequence}')],
      ['\texttt{bracket}', feat('\textsc{Nil}', everyweek)],
      ['\texttt{lex}', feat('\textsc{Nil}\_\tp{of this}', '\textsc{Sequence}')],
      ['\texttt{lex}', feat('\textsc{Nil}\_\tp{of}', '\textsc{Sequence}')],
      ['\texttt{lex}', feat('\textsc{Nil}\_\tp{this}', '\textsc{Sequence}')],
    nil).cmargin(u(1.0)).level(7,8),
    table(
      ['\texttt{bracket}', feat('\textsc{Nil}', '\textsc{Sequence}')],
      ['\texttt{bracket}', feat('\textsc{Nil}', everyweek)],
      ['\texttt{lex}', feat('\textsc{Nil}\_\tp{of this}', everyweek)],
      ['\texttt{lex}', feat('\textsc{Nil}\_\tp{of}', everyweek)],
      ['\texttt{lex}', feat('\textsc{Nil}\_\tp{this}', everyweek)],
    nil).cmargin(u(1.0)).level(8,9),
    table(
      ['\texttt{bracket}', feat('\textsc{Sequence}', '\textsc{Sequence}')],
      ['\texttt{bracket}', feat('\textsc{Intersect}', friday, everyweek)],
    nil).cmargin(u(1.0)).level(9,10),
    table(
      ['\texttt{bracket}', feat('\textsc{Sequence}', '\textsc{Sequence}')],
      ['\texttt{bracket}', feat('\textsc{Intersect}', friday, everyweek)],
      ['\texttt{valid}', feat('\textsc{is\_valid}')],
      ['', '\textit{(e.g., filter \tp{February \th{30}})}'],
    nil).cmargin(u(1.0)).level(10),
  nil)),

nil){ |slide| slide.label('features_intro').slideStyle(slideStyle).signature(43) }

################################################################################
# LEARNING
################################################################################
#slide!('Outline',
#	outline(2),
#nil){ |slide| slide.label('learn_outline').slideStyle(slideStyle).signature(0) }

################################################################################
# TRAINING SETUP
################################################################################
slide!('Training Setup',
	#(training)
	staggeredOverlay(true,
			ctable(h1('Given '), '$\left\{\right.($','$x$',',',time('$y$'), '$)\left.\right\}$'),
			ctable(h1('Given '),
				'$\left\{\right.($',ctable('(',phrase('Phrase'),',',ground('Reference'),')'),
				',',time('Time'),'$)\left.\right\}$'),
		nil),
	pause,
	ind('Not given latent parse'),
	ind('Not given lexical or language cues'),

	#(ambiguity)
	'',
	pause,
	h1('Therefore, in general, multiple parses ground to same time'),
	pause,
	center,
	ctable('( (',phrase('w$_1$ w$_2$'),',',ground(Time.mktime(2013,8,5)),') , ',time(Time.mktime(2013,8,12)), ')'),
	left,
	pause,
	#(ambiguity figure)
	center,
	'','',
	staggeredOverlay(true,
		ambiguousWithText(0),
		ambiguousWithText(1),
		ambiguousWithText(2),
	nil),
	left,
nil){ |slide| slide.label('learn_setup').slideStyle(slideStyle).signature(40) }

################################################################################
# TIMEM K-BEST
################################################################################
slide!('Training',
	emHeaders(0),
	#(input)
	pause,
	center,
	ctable('( (',phrase('next Monday'),',',ground(Time.mktime(2013,8,5)),') , ',time(Time.mktime(2013,8,12)), ')'),
	#(parses)
	'',
	pause,
	staggeredOverlay(true,
		kbest,
		kbest(0),
	nil),
	left,

nil){ |slide| slide.label('learn_timem_kbest').slideStyle(slideStyle).signature(30) }

################################################################################
# TIMEM FILTER
################################################################################
slide!('Training',
	emHeaders(1),
	#(input)
	center,
	ctable('( (',phrase('next Monday'),',',ground(Time.mktime(2013,8,5)),') , ',time(Time.mktime(2013,8,12)), ')'),
	#(parses)
	'',
	staggeredOverlay(true,
		kbest(0),
		kbest(0,true),
		kbest(1,true),
	nil),
nil){ |slide| slide.label('learn_timem_filter').slideStyle(slideStyle).signature(28) }

################################################################################
# TIMEM UPDATE
################################################################################
slide!('Training',
	emHeaders(2),
	center,
	ctable('',phrase(''),ground(''),time('')),
	'',
	staggeredOverlay(true,
		kbest(1,true),
	nil),
nil){ |slide| slide.label('learn_timem_update').slideStyle(slideStyle).signature(8) }

################################################################################
# TIMEM UPDATE2
################################################################################
#slide!('Training',
#	emHeaders(2),
#	'','',
#	#(lex)
#	ctable(
#		'\textbf{$\theta_{\textrm{Lex}}$}', larrow, 
#		ambiguiousLex[0], ',', ambiguiousLex[2], ',', ambiguiousLex[4], ',', ambiguiousLex[5],
#		', $\dots$',
#	nil).cmargin(u(0.1)).rjustify('c'),
#	pause,
#	'',
#	#(grammar)
#	ctable(
#		'\textbf{$\theta_{\textrm{Grammar}}$}', larrow, 
#		ambiguiousGrammar[0], ',', ambiguiousGrammar[1], ',', ambiguiousGrammar[2],
#	nil).cmargin(u(0.1)).rjustify('c'),
#	pause,
#	'',
#	#(time)
#	ctable(
#		'\textbf{$\mu_{\textrm{sequence}},\sigma_{\textrm{sequence}}$}', larrow, 
#		ctable(tuesday, '$+ 0$').rjustify('c'),',',
#		ctable(tuesday, '$+ 1$').rjustify('c'),
#	nil).cmargin(u(0.1)).rjustify('c'),
#nil){ |slide| slide.label('learn_timem_update2').slideStyle(slideStyle).signature(23) }

################################################################################
# TIMEM DETAILS
################################################################################
#slide!('TimEM Discussion',
#	h1('Intuition'),
#	ind('\textit{Bootstrap} from short examples'),
#	pause,
#	ind('\tp{week}, then \tp{next week}, $\dots$'),
#
#	'','',
#	pause,
#	h1('Smoothing'),
#	ind('Dirichlet prior on grammar parameters $\theta$'),
#	ind('Gaussian prior on $\mu$ given MLE $\sigma$'),
#
#	'','',
#	pause,
#	h1('Uniform initialization $\rightarrow$ deterministic'),
#nil){ |slide| slide.label('learn_timem_details').slideStyle(slideStyle).signature(3) }

################################################################################
# RESULTS
################################################################################
#slide!('Outline',
#	outline(3),
#nil){ |slide| slide.label('results_outline').slideStyle(slideStyle).signature(0) }

################################################################################
# CORPUS
################################################################################
slide!('Dataset',
	#(--corpus)
	h1('TempEval2'),
	ind('Newswire annotated for temporal expressions'),
  overlay(
	  ind('6 languages: English, Spanish, Italian, Chinese, Korean, French').level(1,2),
	  ind('6 languages: \textbf{English}, \textbf{Spanish}, Italian, Chinese, Korean, French').level(2),
  nil),
	pause,pause,
  ind('Various sizes: \textbf{1052}, \textbf{1092}, 523, 659, 247, 206'),
	pause,
	'',
	#(--evaluation)
	h1('Evaluation'),
#	ind(ctable('Most likely grounding (e.g., \te{August 5} $\rightarrow$ ', time(Time.mktime(2013,8,5)),')')),
#	pause,
	#(examples)
	ind('\textbf{Type}: Accuracy over result\'s temporal type'),
	ind(ind(ctable(
		range('August 5, 2013'), ' $=$ ', range('August 12, 2013'),
	nil).cmargin(u(0.2)))),
	'',
	pause,
	ind('\textbf{Value}: Accuracy over result\'s value, if types match'),
	ind(ind(ctable(
		range('August 5, 2013'), ' $\ne$ ', range('August 12, 2013'),
	nil).cmargin(u(0.2)))),
	pause,
	#(detail)
	ind('\darkred{Constrained to guess on each example; no contextual cues}'),
nil){ |slide| slide.label('results_results').slideStyle(slideStyle).signature(18) }

################################################################################
# NUMBERS
################################################################################
slide!('Results',
	#(other systems)
	h1('English (all expressions; gold detection)'),
	ind('\textbf{\sys{GUTime}} (Mani and Wilson, 2000)'),
	ind('\textbf{\sys{SUTime}} (Chang and Manning, 2012)'),
	ind('\textbf{\sys{HeidelTime}} (Str\"{o}tgen and Gertz, 2010)'),
	ind('\textbf{\sys{ParsingTime}} (Angeli \textit{et al.}, 2012)'),
  pause,
  '','',
	h1('Spanish'),
	ind('\textbf{\sys{UC3M}} (Vincente-D\\\'iez \textit{et al.} 2010)'),
nil){ |slide| slide.label('results_numbers').slideStyle(slideStyle).signature(47) }

################################################################################
# NUMBERS
################################################################################
slide!('Results',
	#(other systems)
	h1('English (all expressions; gold detection)'),
	center,
	staggeredOverlay(true,
		#(test)
#		results(['gutime']),
#		results(['gutime','sutime']),
#		results(['gutime','sutime','heideltime']),
		results(['gutime','sutime','heideltime','parsingtime']),
		results(['gutime','sutime','heideltime','parsingtime', 'us']),
	nil),
	left,
  pause,
  '',
  h1('Spanish'),
	center,
	staggeredOverlay(true,
		#(test)
#		resultsSpanish(['UC3M']),
		resultsSpanish(['UC3M', 'us']),
	nil),
	left,
nil){ |slide| slide.label('results_english').slideStyle(slideStyle).signature(19) }

################################################################################
# NUMBERS
################################################################################
slide!('Results',
  h1('All Languages (value accuracy)'),
  '',
  center,
	table(
	 ['\darkred{Language}' ,'\darkred{\\# Examples}','\darkred{Train}','\darkred{Test}'],
   ['English', '1052', '0.81', '0.76'],
   ['Spanish', '1092', '0.84', '0.76'],
   pause,
   ['Italian', '523', '0.85', '0.38'],
   ['Chinese', '659', '0.73', '0.60'],
   ['Korean', '247', '0.67', '0.42'],
   ['French', '206', '0.76', '0.35'],
	nil).rmargin(u(0.2)).cmargin(u(0.5)).cjustify('lcc'),
  left,
  '','',
  ind('First results on remaining 4 languages'),
  pause,
  ind('Test accuracy correlates with training size'),
#  ind('High training accuracy: capture multilingual phenomena'),
nil){ |slide| slide.label('results_alllang').slideStyle(slideStyle).signature(9) }

################################################################################
# RESULTS -- USEFUL
################################################################################
slide!('Analysis',
  h1('What are we still missing?'),
nil){ |slide| slide.label('analysis_useful').slideStyle(slideStyle).signature(1) }

################################################################################
# ANALYSIS -- PRAGMATICS
################################################################################
slide!('Analysis',
  ctable('29\\%', h1('Pragmatics')).cmargin(u(0.5)),
  ind(ctable(phrase('Next Saturday?'), '$\rightarrow$', ground(Time.mktime(2013,8,10)), 'or', ground(Time.mktime(2013,8,18)))),
  pause,
  ind(ctable(phrase('Last year?'), '$\rightarrow$', 'a day? a quarter? a year?')),
  pause,
  '',
  ind('Hard even for humans'),
nil){ |slide| slide.label('analysis_pragmatics').slideStyle(slideStyle).signature(7) }

################################################################################
# ANALYSIS -- TYPE ERROR
################################################################################
slide!('Analysis',
  ctable('29\\%', h1('Pragmatics')).cmargin(u(0.5)),
  ctable('16\\%', h1('Type error')).cmargin(u(0.5)),
  ind('Just given \tp{day}'),
  ind(phrase('The past 5 days')),
nil){ |slide| slide.label('analysis_type').slideStyle(slideStyle).signature(0) }

################################################################################
# ANALYSIS -- NUMBERS
################################################################################
slide!('Analysis',
  ctable('29\\%', h1('Pragmatics')).cmargin(u(0.5)),
  ctable('16\\%', h1('Type error')).cmargin(u(0.5)),
  ctable('10\\%', h1('Incorrect number')).cmargin(u(0.5)),
  ind('Drop the number'),
  ind('Spelled out names: \tp{seventeen seventy-six} $\rightarrow$ 17$~~~$76'),
nil){ |slide| slide.label('analysis_numbers').slideStyle(slideStyle).signature(3) }

################################################################################
# ANALYSIS -- RELATIVE RANGE
################################################################################
slide!('Analysis',
  ctable('29\\%', h1('Pragmatics')).cmargin(u(0.5)),
  ctable('16\\%', h1('Type error')).cmargin(u(0.5)),
  ctable('10\\%', h1('Incorrect number')).cmargin(u(0.5)),
  ctable('7\\%$~$', h1('Absolute versus relative ambiguity')).cmargin(u(0.53)),
  center,
  '',
  ind(Parse.new([ctable('\texttt{moveLeft1}(', everymonth, ')'),
    ['\texttt{moveLeft1($-$)}', 'July'],
    [everymonth, '2013']]).constituency),
  pause,
  '',
  '\textit{Local optima}',
  left,
nil){ |slide| slide.label('analysis_relative').slideStyle(slideStyle).signature(17) }

################################################################################
# ANALYSIS -- WRONG
################################################################################
slide!('Analysis',
  ctable('29\\%', h1('Pragmatics')).cmargin(u(0.5)),
  ctable('16\\%', h1('Type error')).cmargin(u(0.5)),
  ctable('10\\%', h1('Incorrect number')).cmargin(u(0.5)),
  ctable('7\\%$~$', h1('Absolute versus relative ambiguity')).cmargin(u(0.53)),
  ctable('19\\%', h1('Other parse errors')).cmargin(u(0.5)),
  pause,
  ctable('19\\%', h1('Out of scope')).cmargin(u(0.5)),
  pause,
  ind(ctable('16\\%', 'Missing context').cmargin(u(0.25))),
  ind(ind(phrase('That time'))),
  ind(ind(phrase('From time to time'))),
  pause,
  ind(ctable('3\\%$~$', 'Bad reference time').cmargin(u(0.28))),
  ind(ind('Annotation error')),
  ind(ind('Reference time is not publication time')),
  
nil){ |slide| slide.label('analysis_wrong').slideStyle(slideStyle).signature(12) }

################################################################################
# CONCLUSION
################################################################################
slide!('Conclusion',
	h1('Multilingual temporal parsing'),
	ind('Compositional grammar of time'),
	ind('Results competitive with state-of-the-art'),
	pause,
	'','','',
	h1('Takeaway points'),
	ind('Multilingual, with no language-specific tuning'),
	ind('Rich features over \textit{types} and \textit{values}'),
	ind('Learns pragmatics of training domain'),
#	ind('Adapt to training domain'),
#  pause,
#  ind(ind('(But, it\'ll adapt to the training domain)')),
nil){ |slide| slide.label('conclusion').slideStyle(slideStyle).signature(18) }

################################################################################
# THANKS
################################################################################
slide!('',
	center,
  '','','','','','','',
	rtable(
		_('Thank You!').scale(2.0).color(darkred),
		'\textit{(\darkblue{Time} for questions)}',
	nil).rmargin(u(0.75)).cjustify('c'),
	left,
nil){ |slide| slide.label('thanks').signature(9) }


################################################################################
# FINISH
################################################################################
finishPresentation





