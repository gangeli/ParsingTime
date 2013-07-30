#!/usr/bin/ruby

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
	leftHeader(image('img/logo.jpg').scale(0.20))

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
		author(_('Gabor Angeli').color(darkblue)),
	nil).rmargin(u(0.75)).cjustify('c'),
	'\small{\textit{with Chris Manning and Dan Jurafsky}}',
	left,
nil){ |slide| slide.label('intro_title').slideStyle(SlideStyle.new.leftHeader(nil)).signature(53) }

################################################################################
# OUTLINE
################################################################################
#slide!('Outline',
#	staggeredOverlay(true,
#		outline(nil),
#		outline(0),
#	nil),
#nil){ |slide| slide.label('intro_outline').signature(13) }

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
nil){ |slide| slide.label('motivation_example').signature(189) }

################################################################################
# EXAMPLE -- PRACTICAL
################################################################################
slide!('Time In Information Extraction',
	h1('News'),
	ind(w('Beginning more than \\textbf{seven hours earlier}, the space station\'s')),
	ind(ind(w('robotic arm detached the 14-foot long Dragon [spacecraft]'))),
	'',
	ind(w('Benjamin Franklin Federal Savings and Loan Association said it')),
	ind(ind(w('plans to restructure in the wake of a \textbf{third-quarter} loss'))),
	'','',
	pause,

	h1('Communication'),
	ind(w('Actually I am on vacation the \textbf{last three weeks of November}')),
	'',
	ind(w('I have some time available at the \textbf{end of next week}')),
nil){ |slide| slide.label('motivation_example_practical').signature(9) }

################################################################################
# MOTIVATION -- PREVIOUS
################################################################################
slide!('Motivation',
	h1('Hand coded rules'),
	ind('\textbf{Rigid}'),
  staggeredOverlay(true,
	  ind(ind('Syntax: \tp{last Friday the \th{12}}')),
	  ind(ind('Syntax: \tp{last Friday the \th{13}}')),
  nil),
  pause,
  staggeredOverlay(true,
	  ind(ind('Domain pragmatics: \tp{A year ago}')),
	  ind(ind('Domain pragmatics: \tp{Sales are down compared to \textbf{a year ago}}')),
	  ind(ind('Domain pragmatics: \tp{Remember? We got married \textbf{a year ago}}')),
  nil),
	'',
	pause,
	ind('\textbf{Rule engineering challenge}'),
	ind(ind('Always more rules: \tp{7 days \textbf{prior}}, \tp{the \textbf{previous} 7 days}')),
	ind(ind('\verb~/the/ /past|last/ (?: ($NUM) /to|-/ )? ($NUM)? ($TEUNITS)~').scale(0.75)),
	ind(ind('\verb~/the/ /next|following/ (?: ($NUM) /to|-/ )? ($NUM)? ($TEUNITS)~').scale(0.75)),
	ind(ind('$\dots$')),

  pause,
  h1(ctable(new, 'Adapt to multiple languages')),
  ind('Annotating data easier than adapting rules'),
nil){ |slide| slide.label('motivation_motiv').signature(40).footnote(
	'\darkblue{Mani \& Wilson 2000; Str\"{o}tgen and Gertz 2010; Chang and Manning (2012)}'
)}

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
nil){ |slide| slide.label('motivation_system').signature(38) }

################################################################################
# MOTIVATION -- US
################################################################################
slide!('Motivation',
	h1('Our Model'),
	ind('\textbf{Gives confidence}'),
	pause,
	ind('\textbf{Elegant representation of time}'),
	pause,
	ind('\textbf{Domain flexible}'),
	ind(ind('Emulates pragmatics of training domain')),
	pause,

	ind('\textbf{Language independent}'),
	ind(ind('\tp{\textbf{Last} Sunday} and \tp{domingo \textbf{pasado}}')),
  pause,
  '',
  staggeredOverlay(true,
    rtable(
      ind(ind('\textbf{Hard}:')),
      ind(ind(ind((ctable(phrase('pasado'), '$\rightarrow$', value('\texttt{last}$($--$)$')))))),
      ind(ind(ind((ctable(phrase('domingo'), '$\rightarrow$', sunday.scale(0.75)))))),
	    ind(ind(ind('\verb~($DOW) /pasad[oa]/~'))),

    nil),
    rtable(
      ind(ind('\textbf{Easier}:')),
      ind(ind(ind(
        ctable('Ask native speaker `when was ', phrase('domingo pasado'), '?\'')))),
    nil),
  nil),
nil){ |slide| slide.label('motivation_motiv2').signature(25) }


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
#nil){ |slide| slide.label('motivation_motiv').signature(46) }

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
#nil){ |slide| slide.label('motivation_related').signature(9) }

################################################################################
# MOTIVATION -- COMPARISON
################################################################################
slide!('Comparison To Lambda Calculus',
	center,
	plet(:startlevel, tstartlevel),
	staggeredOverlay(true,
		_('Parallels to semantic parsing').color(darkred),
		pause,
		_('\textbf{Option 1}: bootstrap from parse').color(darkred),
		pause,
		_('\textbf{Option 2}: bootstrap from grounded interpretation').color(darkred),
	nil).pivot(0,0),
	'',
	plevel(:startlevel),
	staggeredOverlay(true,
		comparison(0,-1,false,false),
		comparison(0, 0,false,false),
		comparison(0, 0,true ,false),
		comparison(0, 0,true ,true),
#		comparison(1, 1,false,false),
		comparison(2, 2,false,false),
		pause,
	nil),
	left,
nil){ |slide| slide.label('motivation_compare').signature(100).footnote(
	_('\darkblue{Zettlemoyer \& Collins 2005/2007; Liang et al. 2011}')
)}

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
#		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1),
		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1),
	nil),
	pause,
	'',
	'',
	#(value)
	ctable('[',ground(Time.mktime(2013,8,5)),']', '$\rightarrow$', time([Time.mktime(2012,8,3),'$-$',Time.mktime(2012,8,5)])),
	left,
nil){ |slide| slide.label('motivation_parse').signature(65) }

################################################################################
# PARSE -- TYPES
################################################################################
slide!('Latent Parse',
	#(intro)
	h1('Nonterminals become very sparse'),
	pause,
	ind(ctable('Consider: ',phrase('last 7 days'),', ',phrase('last 3 months'),', etc.')),
	pause,
	ind('\textbf{Generative Model:} Group nonterminals based on \textit{types}'),
	'',
	#(type parse)
  center,
  staggeredOverlay(true,
  	staggeredOverlay(true,
  		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1),
  #		last2days(:days=>0, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1),
  #		last2days(:days=>0, :two=>0, :twodays=>1, :last=>1, :lasttwodays=>1),
  #		last2days(:days=>0, :two=>0, :twodays=>0, :last=>1, :lasttwodays=>1),
  #		last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>1),
  		last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>0),
  		last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>0, :change=>true),
    nil),
    rtable(
      ctable(lastWeek.scale(0.85), '$\rightarrow$',
          ctable(time(Time.mktime(2013,7,28)), '$-$', time(Time.mktime(2013,8,4)))).rjustify('c'),
      ctable(lastWeek(true).scale(0.85), '$\rightarrow$',
          ctable(time(Time.mktime(2012,8,5)), '$-$', time(Time.mktime(2012,8,6)))).rjustify('c'),
    nil).rmargin(u(0.25)),
  nil),
  left,
  
  #(why it's wrong)
  pause,
  ind('\tp{Last} is not the same function'),
	left,
nil){ |slide| slide.label('motivation_parse2').signature(28) }

################################################################################
# DISCRIMINATIVE MODEL
################################################################################
slide!('Discriminative Model',
	#(features)
	h1('Problem goes away'),
  ind(table(
    ['Coarse-grained features over types:', type('$f:$ Range$\rightarrow$Range')],
    ['Fine grained features over values:', type('\texttt{moveLeft1($-$)}')],
  nil).cmargin(u(0.25))),
	ctable(h1('Adapt semantic parser'), '(e.g., Liang \textit{et al.} 2012)').cmargin(u(0.2)),
  '',
  '',
  pause,
	h1('Popping up the stack'),
  ind(ctable(image('img/check.png').scale(0.04), 'Parsing time is interesting')),
  ind(ctable(image('img/check.png').scale(0.04), 'Latent parse is promising')),
  pause,
  ctable(
    rtable(
      ind(ind('$~$\textbf{Temporal representation}')).level(3),
      ind(ind('$~$Features')),
      ind(ind('$~$Algorithm')),
      ind(ind('$~$Results')),
    nil),
  nil),
  pause,
nil){ |slide| slide.label('discrim').signature(16) }

################################################################################
# REPRESENTATION
################################################################################
#slide!('Outline',
#	outline(1),
#nil){ |slide| slide.label('representation_outline').signature(12) }

################################################################################
# NONTERMINAL TYPES
################################################################################
slide!('Grammar Of Time',
	h1(range),
	'',
	h1(sequence),
	'',
	h1(duration),
	'',
	pause,
	h1(time('Functions')),
	'',
	h1(time('Number')),
	'',
	h1(time('Nil (no temporal meaning)')),
nil){ |slide| slide.label('representation_type_overview').signature(12) }

################################################################################
# RANGE
################################################################################
slide!('Grammar Of Time',
	h1(range),
	ind('A period between two dates (or times)'),
#	'',
#	pause,
#	ind('8 lexical categories: \te{yesterday}, \te{tomorrow}, \te{today}, \te{reference},'),
#	ind(ind('\te{past}, \te{future}, \te{year$(n)$}, \te{century$(n)$}')),
	'',
	ind('\tp{Today}, \tp{June 5 2012}, \tp{day before yesterday}'),
#	'',
#	pause,
#	ind('Interval-based theory of time: instants are ranges'),
#	ind(ind('\te{Today} and \te{Reference} both ranges, but former has duration')),
nil){ |slide| slide.label('representation_range').signature(6) }

################################################################################
# SEQUENCE
################################################################################
slide!('Grammar Of Time',
	h1Grey(range),
	h1(sequence),
	ind('A sequence of Ranges (not necessarily at regular intervals)'),
#	'',
#	pause,
#	ind('44 lexical categories, e.g.:'),
#	ind(ind('\te{DayOfMonth}, \te{DayOfWeek}, \te{WeekOfYear},')),
#	ind(ind('\textit{shorthand}: \te{Friday}, \te{Saturday}, \te{January},')),
#	ind(ind('\textit{dense}: \te{EveryDay}, \te{EveryWeek},')),
	'',
	ind('\tp{June 5}, \tp{last Sunday}, \tp{third quarter}'),
#	'',
#	pause,
#	ind('\red{Still stuck:} which element are we referring to?'),
#	pause,
#	#(answer)
#	'',
#	ind('\green{Answer:} We\'re referring to all of them! (Kind of$\dots$)'),
nil){ |slide| slide.label('representation_sequence').signature(13) }

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
#nil){ |slide| slide.label('representation_sequence_ambiguity').signature(28) }

################################################################################
# DURATION
################################################################################
slide!('Grammar Of Time',
	h1Grey(range),
	h1Grey(sequence),
	h1(duration),
	ind('A period of time: \tp{day}, \tp{2 weeks}, \tp{10 years}'),
#	'',
#	pause,
#	ind('10 lexical categories: \te{second}, \te{minute}, \te{hour}, \te{day}, \te{week}, \te{month},'),
#	ind(ind('\te{quarter}, \te{year}, \te{decade}, \te{century}')),
nil){ |slide| slide.label('representation_duration').signature(13) }

################################################################################
# FUNCTION
################################################################################
def interval(greyed=false, doublesize=false)
  _(doublesize ? '\texttt{[$~~~~~~~~~~~~$)}' : '\texttt{[$~~~~~~$)}').color(greyed ? grey : darkred).bold
end
slide!('Grammar Of Time',
	h1Grey(range),
	h1Grey(sequence),
	h1Grey(duration),
	h1(time('Functions')),
	ind('General sequence and interval operations'),
	'',
	pause,
  staggeredOverlay(true,
    ind(ctable(interval)).scale(1.5),
    ind(overlay(ctable(interval(true)),
                ctable('', interval).cmargin(u(1.0)),
                ctable('\textbf{$~~\rightarrow$}'))).scale(1.5),
    ind(overlay(ctable('', '\textbf{$~~\rightarrow$}').cmargin(u(2.5)),
                ctable('', interval(true)).cmargin(u(1.0)),
                ctable('', interval(false, true)).cmargin(u(1.0)))).scale(1.5),
    ind(overlay(ctable('', interval(true, true)).cmargin(u(1.0)),
                ctable(interval(false, true).color(white), interval).cmargin(u(1.0)))).scale(1.5),
    ind('Move a sequence, multiply a duration, etc.'),
  nil),
  center,
  '','',
  staggeredOverlay(true,
    rtable(phrase('a year \textbf{from} today'),
           '...or, \tp{\textbf{next} year}?').level(2).rmargin(u(0.5)),
    rtable(phrase('Monday \textbf{through} Friday')).level(3),
    rtable(phrase('The day \textbf{after} tomorrow')).level(4),
    rtable(phrase('\textbf{next} year'),
           phrase('5 years'),
           phrase('$\dots$')).level(5),
  nil),
nil){ |slide| slide.label('representation_function').signature(47) }

################################################################################
# NUMBER
################################################################################
slide!('Grammar Of Time',
	h1Grey(range),
	h1Grey(sequence),
	h1Grey(duration),
	h1Grey(time('Functions')),
	h1(time('Number')),
	ind('A number, characterized by its ordinality and magnitude'),
nil){ |slide| slide.label('representation_number').signature(3) }

################################################################################
# NIL
################################################################################
slide!('Grammar Of Time',
	h1Grey(range),
	h1Grey(sequence),
	h1Grey(duration),
	h1Grey(time('Functions')),
	h1Grey(time('Number')),
	h1(time('Nil')),
	ind('A word without direct temporal meaning'),
	ind(ind(phrase('a week'))),
	ind(ind(phrase('the week'))),
nil){ |slide| slide.label('representation_nil').signature(6) }

################################################################################
# COMBINATION
################################################################################
slide!('Grammar Of Time',
	h1Grey(range),
	h1Grey(sequence),
	h1Grey(duration),
	h1Grey(time('Functions')),
	h1Grey(time('Number')),
	h1Grey(time('Nil')),
	h1('Combination Rules'),
	ind('Predefined combination rules'),
	ind('Type checking function application'),
nil){ |slide| slide.label('representation_combination').signature(2) }

################################################################################
# FEATURES
################################################################################
slide!('Features',
  h1('TODO'),
nil){ |slide| slide.label('representation_combination').signature(0) }

################################################################################
# LEARNING
################################################################################
#slide!('Outline',
#	outline(2),
#nil){ |slide| slide.label('learn_outline').signature(0) }

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
	ind('Not given lexical cues'),

	#(ambiguity)
	'',
	pause,
	h1('Therefore, in general, latent parse is ambiguous'),
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
nil){ |slide| slide.label('learn_setup').signature(39) }

################################################################################
# TIMEM K-BEST
################################################################################
slide!('Training: TimEM',
	emHeaders(0),
	#(input)
	pause,
	center,
	ctable('( (',phrase('next Monday'),',',ground(Time.mktime(2012,6,5)),') , ',time(Time.mktime(2012,6,12)), ')'),
	#(parses)
	'',
	pause,
	staggeredOverlay(true,
		kbest,
		kbest(0),
	nil),
	left,

nil){ |slide| slide.label('learn_timem_kbest').signature(25) }

################################################################################
# TIMEM FILTER
################################################################################
slide!('Training: TimEM',
	emHeaders(1),
	#(input)
	center,
	ctable('( (',phrase('next Monday'),',',ground(Time.mktime(2012,6,5)),') , ',time(Time.mktime(2012,6,12)), ')'),
	#(parses)
	'',
	staggeredOverlay(true,
		kbest(0),
		kbest(0,true),
		kbest(1,true),
	nil),
nil){ |slide| slide.label('learn_timem_filter').signature(25) }

################################################################################
# TIMEM UPDATE
################################################################################
slide!('Training: TimEM',
	emHeaders(2),
	center,
	ctable('',phrase(''),ground(''),time('')),
	'',
	staggeredOverlay(true,
		kbest(1,true),
	nil),
nil){ |slide| slide.label('learn_timem_update').signature(7) }

################################################################################
# TIMEM UPDATE2
################################################################################
slide!('Training: TimEM',
	emHeaders(2),
	'','',
	#(lex)
	ctable(
		'\textbf{$\theta_{\textrm{Lex}}$}', larrow, 
		ambiguiousLex[0], ',', ambiguiousLex[2], ',', ambiguiousLex[4], ',', ambiguiousLex[5],
		', $\dots$',
	nil).cmargin(u(0.1)).rjustify('c'),
	pause,
	'',
	#(grammar)
	ctable(
		'\textbf{$\theta_{\textrm{Grammar}}$}', larrow, 
		ambiguiousGrammar[0], ',', ambiguiousGrammar[1], ',', ambiguiousGrammar[2],
	nil).cmargin(u(0.1)).rjustify('c'),
	pause,
	'',
	#(time)
	ctable(
		'\textbf{$\mu_{\textrm{sequence}},\sigma_{\textrm{sequence}}$}', larrow, 
		ctable(tuesday, '$+ 0$').rjustify('c'),',',
		ctable(tuesday, '$+ 1$').rjustify('c'),
	nil).cmargin(u(0.1)).rjustify('c'),
nil){ |slide| slide.label('learn_timem_update2').signature(23) }

################################################################################
# TIMEM DETAILS
################################################################################
slide!('TimEM Discussion',
	h1('Intuition'),
	ind('\textit{Bootstrap} from short examples'),
	pause,
	ind('\tp{week}, then \tp{next week}, $\dots$'),

	'','',
	pause,
	h1('Smoothing'),
	ind('Dirichlet prior on grammar parameters $\theta$'),
	ind('Gaussian prior on $\mu$ given MLE $\sigma$'),

	'','',
	pause,
	h1('Uniform initialization $\rightarrow$ deterministic'),
nil){ |slide| slide.label('learn_timem_details').signature(3) }

################################################################################
# RESULTS
################################################################################
#slide!('Outline',
#	outline(3),
#nil){ |slide| slide.label('results_outline').signature(0) }

################################################################################
# CORPUS
################################################################################
slide!('Dataset',
	#(--corpus)
	h1('TempEval2'),
	ind('Newswire annotated for temporal expressions'),
	pause,
	ind('1052 training / 156 test expressions'),
	pause,
	'','',
	#(--evaluation)
	h1('Evaluation'),
	ind(ctable('Most likely grounding (e.g., \te{June 5} $\rightarrow$ ', time(Time.mktime(2012,6,5)),')')),
	pause,
	#(examples)
	ind('\textbf{Type}: Accuracy over result\'s temporal type'),
	ind(ind(ctable(
		range('June 5, 2012'), ' $=$ ', range('June 12, 2012'),
	nil).cmargin(u(0.2)))),
	'',
	pause,
	ind('\textbf{Value}: Accuracy over result\'s value, if types match'),
	ind(ind(ctable(
		range('June 5, 2012'), ' $\ne$ ', range('June 12, 2012'),
	nil).cmargin(u(0.2)))),
	pause,
	#(detail)
	ind('\darkred{Constrained to guess on each example; no contextual cues}'),
nil){ |slide| slide.label('results_results').signature(10) }

################################################################################
# NUMBERS
################################################################################
slide!('Results',
	#(other systems)
	h1('Comparisons'),
	ind('\textbf{\sys{GUTime}} (Mani and Wilson, 2000)'),
	ind('\textbf{\sys{SUTime}} (Chang and Manning, 2012)'),
	ind('\textbf{\sys{HeidelTime}} (Str\"{o}tgen and Gertz, 2010)'),
	'','',
	pause,
	
	#(results)
	h1('Test (all expressions; gold detection)').level(1),
	'',
	center,
	staggeredOverlay(true,
		#(test)
		results(true,['gutime']),
		results(true,['gutime','sutime']),
		results(true,['gutime','sutime','heideltime']),
		results(true,['gutime','sutime','heideltime','parsingtime']),
#		#(training)
#		results(false,['gutime','sutime','heideltime','parsingtime']),
	nil),
	left,
nil){ |slide| slide.label('results_numbers').signature(43) }

################################################################################
# CONCLUSION
################################################################################
slide!('Conclusion',
	h1('Probabilistic, compositional temporal parsing'),
	ind('Elegant compositional representation of time'),
	ind('EM-like algorithm for learning'),
	pause,
	ind('Results competitive with state-of-the-art'),
	pause,
	'',
	h1('Takeaway points'),
	ind('Capture \textbf{syntactic} ambiguity: \tp{last Friday the \th{13}}'),
	ind('Capture \textbf{pragmatic} ambiguity: \tp{last Sunday}'),
	pause,
	ind('\textbf{Robust} guess and confidence for any expression'),
	pause,
	ind('\textbf{Language/domain independent} approach'),
#	pause,
#	'',
#	h1('Future directions'),
#	ind('Multilingual support'),
#	ind('Incorporate contextual information'),
#	pause,
#	ind('Similar approach for spatial descriptions?'),
nil){ |slide| slide.label('conclusion').signature(11) }

################################################################################
# THANKS
################################################################################
slide!('',
	center,
	rtable(
		image('img/logo.jpg').scale(0.25),
		_('Thank You!').scale(2.0).color(darkred),
		'\textit{(We\'re out of \darkblue{time})}',
	nil).rmargin(u(0.75)).cjustify('c'),
	left,
nil){ |slide| slide.label('thanks').slideStyle(SlideStyle.new.leftHeader(nil)).signature(6) }


################################################################################
# FINISH
################################################################################
finishPresentation





