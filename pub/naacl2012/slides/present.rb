#!/usr/bin/ruby

require 'rfig/Presentation'
require 'figlib.rb'
require 'include.rb'

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
nil){ |slide| slide.label('intro_title').slideStyle(SlideStyle.new.leftHeader(nil)).signature(52) }

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
nil){ |slide| slide.label('motivation_example').signature(180) }

################################################################################
# EXAMPLE -- PRACTICAL
################################################################################
slide!('Time In Information Extraction',
	h1('News'),
	ind(w('Beginning more than \\textbf{seven hours earlier}, the space station\'s')),
	ind(ind(w('robotic arm detached the 14-foot long Dragon [spacecraft]'))),
	ind(w('Benjamin Franklin Federal Savings amp Loan Association said it')),
	ind(ind(w('plans to restructure in the wake of a \textbf{third-quarter} loss'))),
	'','',
	pause,

	h1('Communication'),
	ind(w('Actually I am on vacation the \textbf{last three weeks of November}')),
	ind(w('I have some time available at the \textbf{end of next week}')),
nil){ |slide| slide.label('motivation_example_practical').signature(7) }

################################################################################
# MOTIVATION -- PREVIOUS
################################################################################
slide!('Motivation',
	h1('Current approaches'),
	ind('Collection of hand-coded rules'),
	'',
	pause,
	h1('Downsides'),
	ind('Rigid syntax: \tp{last Friday the \th{13}}'),
	'',
	pause,
	ind('Rigid pragmatics: when is \tp{last Sunday}?'),
	'',
	pause,
	ind('Always more rules: \tp{7 days \textbf{prior}}, \tp{the \textbf{previous} 7 days}'),
	'',
	pause,
	ind('Rule engineering challenge'),
	ind(ind('\verb~/the/ /past|last/ (?: ($NUM) /to|-/ )? ($NUM)? ($TEUNITS)~').scale(0.75)),
	ind(ind('\verb~/the/ /next|following/ (?: ($NUM) /to|-/ )? ($NUM)? ($TEUNITS)~').scale(0.75)),
	ind(ind('\verb~/another/ (?: ($NUM) /to|-/ )? ($NUM)? ($TEUNITS)~').scale(0.75)),
	ind(ind('$\dots$')),
nil){ |slide| slide.label('motivation_motiv').signature(21).footnote(
	'\darkblue{Mani \& Wilson 2000; Str\"{o}tgen and Gertz 2010; Chang and Manning (2012)}'
)}

################################################################################
# MOTIVATION -- US
################################################################################
slide!('Motivation',
	h1('Probabilistic Model'),
	ind('Handle \textit{syntactic} ambiguity'),
	ind(ind('\tp{[Last Friday] [the \th{13}]} and \tp{[last] [Friday the \th{13}]}')),
	'',
	pause,
	ind('Handle \textit{pragmatic} ambiguity'),
	ind(ind('\tp{Could we meet on Tuesday?}')),
	'',
	pause,
	ind('\textit{Language/domain flexible}'),
	ind(ind('\tp{\textbf{Last} Friday} and \tp{viernes \textbf{pasado}}')),
	'',
	pause,
	ind('Plays nice in larger systems'),
	ind(ind('Confidence and guess for any expression')),
	ind(ind('Learn any structure seen in training')),
nil){ |slide| slide.label('motivation_motiv2').signature(5) }

################################################################################
# MOTIVATION -- COMPARISON
################################################################################
slide!('Inspiration',
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
nil){ |slide| slide.label('motivation_compare').signature(96).footnote(
	_('\darkblue{Zettlemoyer \& Collins 2005/2007; Liang et al. 2011}')
)}


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
#		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1),
		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1),
	nil),
	pause,
	'',
	'',
	#(value)
	ctable('[',ground(Time.mktime(2012,6,5)),']', '$\rightarrow$', time([Time.mktime(2012,6,3),'$-$',Time.mktime(2012,6,5)])),
	left,
nil){ |slide| slide.label('motivation_parse').signature(64) }

################################################################################
# PARSE -- TYPES
################################################################################
slide!('Latent Parse',
	#(intro)
	h1('Nonterminals become very sparse'),
	pause,
	ind(ctable('Consider: ',phrase('last 7 days'),', ',phrase('last 3 months'),', etc.')),
	pause,
	ind('\textbf{Solution:} Group nonterminals based on \textit{types}'),
	pause,
	'',
	#(type parse)
	center,
	staggeredOverlay(true,
		last2days(:days=>1, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1),
		last2days(:days=>0, :two=>1, :twodays=>1, :last=>1, :lasttwodays=>1),
		last2days(:days=>0, :two=>0, :twodays=>1, :last=>1, :lasttwodays=>1),
#		last2days(:days=>0, :two=>0, :twodays=>0, :last=>1, :lasttwodays=>1),
#		last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>1),
		last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>0),
		last2days(:days=>0, :two=>0, :twodays=>0, :last=>0, :lasttwodays=>0, :change=>true),
	nil),
	left,
	'',
	pause,
	'What are these nonterminals, and how do they combine?',
nil){ |slide| slide.label('motivation_parse2').signature(14) }

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
	pause,
	ind('\tp{June 5, 2012}, \tp{November 1987}, \tp{day before yesterday}'),
	'',
	pause,
	ind('Interval-based theory of time: instants are ranges'),
	ind(ind('\te{Today} and \te{Reference} both ranges, but former has duration')),
nil){ |slide| slide.label('representation_range').signature(3) }

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
	pause,
	ind('\tp{June 5}, \tp{next Saturday}, \tp{third quarter}'),
	'',
	pause,
	ind('\red{Still stuck:} which element are we referring to?'),
	pause,
	#(answer)
	'',
	ind('\green{Answer:} We\'re referring to all of them! (Kind of$\dots$)'),
nil){ |slide| slide.label('representation_sequence').signature(11) }

################################################################################
# SEQUENCE AMBIGUITY
################################################################################
slide!('Grammar Of Time',
	h1Grey(range),
	h1(sequence),
	#(distribution)
	'',
	ind(ctable('Today is',ground(Time.mktime(2012,6,5)),', what is \tp{next Saturday}?')),
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
	#(characterize Gaussian)
	'',
	pause,
	ind('Construct a distribution over possible groundings'),
	ind(ind('Appealing to model as a Gaussian')),
	pause,
	ind(ind('Learn Gaussian parameters $\mu,\sigma$')),
nil){ |slide| slide.label('representation_sequence_ambiguity').signature(24) }

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
nil){ |slide| slide.label('representation_duration').signature(12) }

################################################################################
# FUNCTION
################################################################################
slide!('Grammar Of Time',
	h1Grey(range),
	h1Grey(sequence),
	h1Grey(duration),
	h1(time('Functions')),
	ind('General sequence and interval operations'),
	'',
	pause,
	ind('Examples:'),
	ind(ind('Shift a range/sequence by a duration [\tp{3 days ago}]')),
	ind(ind('Move the origin of a sequence [\tp{last week}]')),
	ind(ind('Take the \th{$n$} element of a sequence [\tp{second week of June}]')),
	ind(ind('$\dots$')),
nil){ |slide| slide.label('representation_function').signature(5) }

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
nil){ |slide| slide.label('representation_number').signature(1) }

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
	pause,
	ind('Lexicalized: e.g., \te{Nil-the}, \te{Nil-a}'),
	pause,
	ind(ind(phrase('a week'))),
	ind(ind(phrase('the week'))),
nil){ |slide| slide.label('representation_nil').signature(3) }

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
	ctable('( (',phrase('w$_1$ w$_2$'),',',ground(Time.mktime(2012,6,5)),') , ',time(Time.mktime(2012,6,12)), ')'),
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
nil){ |slide| slide.label('learn_setup').signature(34) }

################################################################################
# TIMEM K-BEST
################################################################################
slide!('Training: TimEM',
	emHeaders(0),
	#(input)
	pause,
	center,
	ctable('( (',phrase('next Tuesday'),',',ground(Time.mktime(2012,6,5)),') , ',time(Time.mktime(2012,6,12)), ')'),
	#(parses)
	'',
	pause,
	staggeredOverlay(true,
		kbest,
		kbest(0),
	nil),
	left,

nil){ |slide| slide.label('learn_timem_kbest').signature(24) }

################################################################################
# TIMEM FILTER
################################################################################
slide!('Training: TimEM',
	emHeaders(1),
	#(input)
	center,
	ctable('( (',phrase('next Tuesday'),',',ground(Time.mktime(2012,6,5)),') , ',time(Time.mktime(2012,6,12)), ')'),
	#(parses)
	'',
	staggeredOverlay(true,
		kbest(0),
		kbest(0,true),
		kbest(1,true),
	nil),
nil){ |slide| slide.label('learn_timem_filter').signature(24) }

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
	h1('Test (all expressions)').level(1),
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
nil){ |slide| slide.label('results_numbers').signature(38) }

################################################################################
# CONCLUSION
################################################################################
slide!('Conclusion',
	h1('Probabilistic, compositional temporal parsing'),
	ind('Share parameters over temporal types'),
	ind('EM-like algorithm for learning'),
	pause,
	ind('Results competitive with state-of-the-art'),
	pause,
	'',
	h1('Takeaway points'),
	ind('Capture \textbf{syntactic} ambiguity: \tp{last Friday the \th{13}}'),
	ind('Capture \textbf{Pragmatic} ambiguity: \tp{next Friday}'),
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
nil){ |slide| slide.label('conclusion').signature(8) }

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





