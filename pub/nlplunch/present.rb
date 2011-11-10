#!/usr/bin/ruby

require 'rfig/Presentation'
require "#{ENV['HOME']}/workspace/time/aux/figlib.rb"

"""
	Point-based versus Interval-based semantics of time
	'Temporal Semantics'
	Look at TempEval2 guidelines

	Reference in Time:Ranges is x=>x
	Time: Primitives as Lambdas
		explain how we find 0 point ('with respect to enclosing container')
		reword 'offset not mentioned'
	Training: Data better presentation of ambiguity
"""

#--Set Slide Style
slideStyle = nil
slideStyle = SlideStyle.new.border(1).borderColor(black)

#--Init Presentation
initPresentation(
	:latexHeader => IO.readlines("#{ENV['HOME']}/lib/latex/std-macros.tex"),
	:edition => 'present',
	:fontSize => 32
).slideStyle(slideStyle)

#def now; image('img/now2.jpg').scale(0.08); end
def phrase(txt); _("\\textit{#{txt}}").color(darkgreen); end
def ground(time); _("\\texttt{#{time}}").color(blue); end
def time(time); _("\\texttt{#{time}}").color(darkred); end
def now; time('$x$'); end
def interval(start,duration,finish)
	overlay(
		ctable(
			rtable(
				l=_('$\mid$'),
				_(start).scale(0.75),
			nil).cjustify('c'),
			'   ',_(duration).color(white),'   ',
			rtable(
				r=_('$\mid$'),
				_(finish).scale(0.75),
			nil).cjustify('c'),
		nil),
		p=path(tcenter(l),tcenter(r)).thickness(2).color(black),
		shift( tleft(p).post{ |x| x.add(upair(0.25,0.07)) } ),
		time(duration).scale(0.75),
	nil)
end
#def range(start,finish)
#	a = (start.is_a?(String) ? time(start) : start)
#	b = (finish.is_a?(String) ? time(finish) : finish)
#	ctable('[',a,',',b,')').rjustify('c')
#end
def range(start,finish)
	interval(time(start),'',time(finish))
end
def urange(duration,direction=1)
	if direction >= 0 then
		interval(now,duration,'')
	else
		interval('',duration,now)
	end
end
################################################################################
# DEBUG
################################################################################
#slide!('test',
#	urange('1 day',-1),
#nil){ |slide| slide.label('test').signature(40) }

################################################################################
# LIB
################################################################################
def rangeFig(color=black,textVal=nil)
	overlay(
		ctable(
			l=_('$\left[\right.$').color(color),
			t = textVal ? _(textVal).color(color) : nil,
			r=_('$\left.\right)$').color(color),
		nil).center,
		path(tcenter(l),textVal ? tleft(t) : tcenter(r)).thickness(1).color(color),
		textVal ? path(tright(t),tcenter(r)).thickness(1).color(color) : nil,
	nil)
end

def cat
	ctable(
	overlay(
		ctable(
			l=_('$\left[\right.$').color(blue),
			r=_('$\left.\right)$').color(darkgrey),
		nil).center,
		path(tcenter(l),tcenter(r)).thickness(1).color(blue),
	nil),
	overlay(
		ctable(
			l=_('$\left[\right.$').color(darkgrey),
			r=_('$\left.\right)$').color(blue),
		nil).center,
		path(tcenter(l),tcenter(r)).thickness(1).color(blue),
	nil),
	nil)
end

def intersect(a,b)
	ctable(a,'$\cap$',b).cjustify('c')
end

def shiftLeft(range=nil,duration="",active=true)
	overlay(
		t = ctable(
			l=rangeFig(active ? blue : darkgrey,nil),
			_('$\dots$').scale(0.5),
			r=(range.is_a?(String) or not range) ? rangeFig(darkgrey,range) : range,
		nil).center,
		p = path(
			tup(r),
			tup(t).post{ |x| x.add(upair(0.0,0.2)) },
			tup(l),
		nil).arrow.curved.dashed('evenly').color(darkgrey),
		shift( tup(p).post{ |x| x.add(upair(-0.2,0.07)) } ),
		ctable(
			_(duration).scale(0.5).color(active ? blue : darkgrey),
		nil).opaque,
	nil)
end

def shiftRight(range=nil,duration="",active=true)
	overlay(
		t = ctable(
			r=(range.is_a?(String) or not range) ? rangeFig(darkgrey,range) : range,
			_('$~~~$').scale(0.5),
			l=rangeFig(active ? blue : darkgrey,nil),
		nil).center,
		p = path(
			tup(r),
			tup(t).post{ |x| x.add(upair(0.0,0.2)) },
			tup(l),
		nil).arrow.curved.dashed('evenly').color(darkgrey),
		shift( tup(p).post{ |x| x.add(upair(-0.2,0.07)) } ),
		ctable(
			_(duration).scale(0.5).color(active ? blue : darkgrey),
		nil).opaque,
	nil)
end

def catRight(range=nil,duration="")
	overlay(
		t = ctable(
			r=(range.is_a?(String) or not range) ? rangeFig(darkgrey,range) : range,
			l=rangeFig(blue,nil),
		nil).cmargin(u(-0.05)).center,
		p = path(
			tup(r),
			tup(t).post{ |x| x.add(upair(0.0,0.2)) },
			tup(l),
		nil).arrow.curved.dashed('evenly').color(darkgrey),
		shift( tup(p).post{ |x| x.add(upair(-0.2,0.07)) } ),
		ctable(
			_(duration).scale(0.5).color(blue),
		nil).opaque,
	nil)
end

def shrinkEnd(range=nil,duration=nil)
	overlay(
		tbl = ctable(
			farL=_('$\left[\right.$').color(darkgrey),
			dots=_('$\dots$').scale(0.5),
			l=_('$\left[\right.$').color(blue),
			t = duration ? _(duration).color(blue) : nil,
			ctable(
				r=_('$\left.\right)$').color(blue),
				farR=_('$\left.\right)$').color(darkgrey),
			nil).cmargin(u(-0.1)),
		nil).center,
		path(tcenter(l),t ? tleft(t) : tcenter(r)).thickness(1).color(blue),
		t ? path(tright(t),tcenter(r)).thickness(1).color(blue) : nil,
		p = path(
			tup(dots).post{ |x| x.add(upair(0.0,0.1)) },
			tup(tbl).post{ |x| x.add(upair(0.0,0.2)) },
			t ? tup(t) : tup(l).post{ |x| x.add(upair(0.1,0.0)) },
		nil).arrow.curved.dashed('evenly').color(darkgrey),
	nil)
end

def calendar(txt,img)
	ctable(
		image(img).scale(0.025),
		time(txt),
	nil)
end
def clock(txt,img)
	ctable(
		image(img).scale(0.1),
		time(txt),
	nil)
end
def dom(n); calendar("$#{n}^{\\textrm{th}}$",'img/months.png'); end
def domrd(n); calendar("$#{n}^{\\textrm{rd}}$",'img/months.png'); end
def thursday; calendar('THU','img/months.png'); end
def friday; calendar('FRI','img/months.png'); end
def monday; calendar('MON','img/months.png'); end
def tuesday; calendar('TUE','img/months.png'); end
def jan; calendar('JAN','img/months.png'); end
def apr; calendar('APR','img/months.png'); end
def nov; calendar('NOV','img/months.png'); end
def sec; clock('SEC','img/times.png'); end
def hour; clock('HOUR','img/times.png'); end
def century; calendar('CENTURY','img/months.png'); end
	
def darrow; image('img/darrow.jpg').scale(0.05); end
def uarrow; image('img/uarrow.jpg').scale(0.05); end
def larrow; image('img/larrow.jpg').scale(0.05); end
def rarrow; image('img/rarrow.jpg').scale(0.05); end

def describe(*rows)
	table(*rows.map{ |name,desc|
		name ? [_(name).color(darkblue), desc] : nil
	}).cmargin(u(1.0)).rmargin(u(0.2)).rjustify('c')
end
################################################################################
# FIGS
################################################################################
def lastFriday
	Parse.new(
		[ctable( shiftLeft(friday,'1'), '$\cap$', dom(13) ).rjustify('c'),
			[shiftLeft(friday,'1'),
				[shiftLeft(rangeFig(blue),'1'),'last'],
				[friday,'friday'],
			],
			[dom(13),
				['nil','the'],
				[dom(13),
					[dom(13),'13'],
					['nil','th'],
				],
			],
		]
	).constituency
end
def lastFriday13
	Parse.new(
		[shiftLeft(intersect(friday,dom(13)),'1'),
			[shiftLeft(rangeFig(blue),'1'),'last'],
			[intersect(friday,dom(13)),
				[friday,'friday'],
				[dom(13),
					['nil','the'],
					[dom(13),
						[dom(13),'13'],
						['nil','th'],
					],
				],
			],
		]
	).constituency
end
def lastFridayTypes
	Parse.new(
		['\texttt{Sequence}',
			['\texttt{Sequence}',
				['$f(\texttt{Sequence}):\texttt{Sequence}$','last'],
				['\texttt{Sequence}','Friday'],
			],
			['\texttt{Sequence}',
				['\texttt{Nil}','the'],
				['\texttt{Sequence}',
					['\texttt{Sequence}','13'],
					['\texttt{Nil}','th'],
				]
			],
	]).constituency
end
def lastFriday13Types
	Parse.new(
		['\texttt{Sequence}',
			['$f(\texttt{Sequence}):\texttt{Sequence}$','last'],
			['\texttt{Sequence}',
				['\texttt{Sequence}','Friday'],
				['\texttt{Sequence}',
					['\texttt{Nil}','the'],
					['\texttt{Sequence}',
						['\texttt{Sequence}','13'],
						['\texttt{Nil}','th'],
					],
				]
			],
		]
	).constituency
end
def thisWeek
	Parse.new(
		['\texttt{Sequence}',
			['\texttt{Nil}','this'],
			['\texttt{Sequence}','week']
		]).constituency
end
def aWeek
	Parse.new(
		['\texttt{Duration}',
			['\texttt{Nil}','a'],
			['\texttt{Duration}','week']
		]).constituency
end
def may13
	Parse.new(
		['\texttt{Sequence}',
			['\texttt{Sequence}','May'],
			['\texttt{Sequence}','13']
		]).constituency
end
def may2011
	Parse.new(
		['\texttt{Range}',
			['\texttt{Sequence}','May'],
			['\texttt{Range}','2011']
		]).constituency
end

def fridayDist
	DataTable.new(:cellName => 'Probability',
  :colName => 'Offset',
  :colLabels => [
		rtable(-2,time('10/28/11')).cjustify('c'),
		rtable(-1,time('11/4/11')).cjustify('c'),
		rtable(0,time('11/11/11')).cjustify('c'),
		rtable(1,time('11/18/11')).cjustify('c'),
		rtable(2,time('11/25/11')).cjustify('c')],
  :contents => [[0.1], [0.2], [0.4], [0.15], [0.05]].transpose)
end

################################################################################
# TITLE
################################################################################
slide!('',
	center,
	rtable(
		image('img/logo.jpg').scale(0.25),
		_('It\'s Parsing Time!').scale(2.0).color(darkblue),
		_('Gabor Angeli'),
	nil).rmargin(u(0.5)).cjustify('c'),
	left,
nil){ |slide| slide.label('title').signature(8) }

################################################################################
# OUTLINE 
################################################################################
slide!('\textbf{Outline}',
	'','',
	_('1. Task Description').color(darkblue),
	'','',
	_('2. Temporal Framework').color(darkblue),
	'','',
	_('3. Parsing and Training').color(darkblue),
	'','',
	_('4. Data and Results \textit{(preliminary)}').color(darkblue),
nil){ |slide| slide.label('outline').signature(8) }

################################################################################
# TASK -- overview
################################################################################
slide!('\textbf{Task:} Overview',
	'',
	#task
	center,
	_('Resolve temporal phrases into their \textit{grounded} time,').bold,
	_('given a reference time').bold,
	left,
	'',
	'',
	pause,
	#input
	ctable(
		_('Input: ').color(darkblue),
		ctable('$x=($',phrase('$w$'),',',ground('$r$'),'$)$'),
	nil),
	ind(ctable(phrase('$w$'),' = A phrase denoting a temporal expression')),
	ind(ctable(ground('$r$'),' = A reference time')),
	center,
	ctable('(',phrase('Last Friday the 13 th'),',',ground('May 16 2011'),')'),
	left,
	'',
	pause,

	#output
	ctable(
		_('Output: ').color(darkblue),
		ctable(time('$y$'), ', the time being referred to'),
	nil),
	center,
	time('\texttt{May 13 2011}'),
	left,
nil){ |slide| slide.label('task_overview').signature(26) }

################################################################################
# TASK -- approach
################################################################################

slide!('\textbf{Task:} Approach',
	#task
	center,
	_('In reality, each expression has a latent parse').bold,
	left,
	'','',
	pause,

	#pipeline
	center,
	#(input)
	ctable(
		'(',
		phrase('Last Friday the 13 th'),
		',',
		ground('May 16 2011'),
		')',
	nil).center,
	darrow,
	#(parse)
	lastFriday.scale(0.75),
	darrow,
	#(output)
	time('May 13 2011'),
	left,
nil){ |slide| slide.label('task_approach').signature(41) }

################################################################################
# APPROACH -- prior work
################################################################################
slide!('\textbf{Approach:} Prior Work',
	_('Rule Based Systems').color(darkblue),
		ind('Benchmark to compare against').color(darkgreen),
		ind('e.g. GUTime, SUTime'),
		ind('\texttt{http://nlp.stanford.edu:8080/sutime/}'),
	pause,
	'','','',
	_('Parsing to Logic').color(darkblue), 
		ind('Inspirations for approach').color(darkgreen),
		ind('CCG: (Zettlemoyer and Colins 2005/2007)'),
		ind('Dependency-based Compositional Semantics: (Liang et al 2011)'),
nil){ |slide| slide.label('approach_rulebased').signature(4) }

################################################################################
# APPROACH -- advantages
################################################################################
slide!('\textbf{Approach:} Advantages',
#	center,'\textit{(more on each of these later)}',left,

	_('Handle [Parsing] Ambiguity').color(darkblue),
	ind('Like regular parsing, a phrase can be syntactically ambiguous'),
	ind(ctable(phrase('Last Friday the 13 th'), rarrow, time('May 13 2011'),'?')),
	ind(ctable(phrase('Last Friday the 13 th'), rarrow, time('Aug 13 2010'),'?')),
	pause,

	_('Handle [Pragmatic] Ambiguity').color(darkblue),
	ind('Often it is not clear what a time a temporal notion refers to'),
	ind(ctable(phrase('Next Friday'), rarrow, time('Nov 11 2011'),'?')),
	ind(ctable(phrase('Next Friday'), rarrow, time('Nov 18 2011'),'?')),
	pause,
	
	_('Propagates Uncertainty').color(darkblue),
	pause,

	_('Grammar is Language Agnostic').color(darkblue),
	ind(ctable(
		phrase('Next Friday'), 'and', phrase('Viernes pr\`{o}ximo'),
		'use same time constructs',
		nil)),
nil){ |slide| slide.label('approach_advantages').signature(9) }

################################################################################
# TIME -- theory
################################################################################
slide!('\textbf{Time:} Theory',
	#main idea
	center,
	_('Temporal expressions can be represented by a finite set of').bold,
	_('functions applied to a finite set of primitives').bold,
	left,
	
	'','','',
	
	ctable(
		#sample parse
		lastFriday.scale(0.75),
		pause,
		#description of terms
		rtable(
			#functions
			_('Fuctions:').color(darkblue),
			table(
				[shiftLeft(rangeFig(blue),'1').scale(0.75),
					'Move back in the sequence'],
				['$\cap$',
					'Take overlap of two times'],
			nil).cmargin(u(0.3)),
			'','','',
			pause,
			#primitives
			_('Primitives:').color(darkblue),
			table(
				[friday.scale(0.75),'(Every) Friday'],
				[dom(13).scale(0.75),'13$^\textrm{th}$ day of (every) month'],
				['nil', '\textit{Glue} word'],
			nil).cmargin(u(0.3)),
		nil),
	nil).cmargin(u(0.5)),
nil){ |slide| slide.label('time_theory').signature(13) }

################################################################################
# TIME -- types
################################################################################
slide!('\textbf{Time:} Primitive Types',
	ctable(_('Time').color(darkblue), ': A moment in time'),
		ind(ctable('e.g. ',time('Nov 10 2011 13:24.427'))),
	pause,

	ctable(_('Range').color(darkblue), ': An interval of time'),
		ind(ctable(phrase('March, 2012'),rarrow,
			range('Mar 1 2012','Apr 1 2012')).rjustify('c')),
		ind(ctable(phrase('Today'),rarrow,urange('1 day')
			).rjustify('c')),
	pause,
	
	ctable(_('Duration').color(darkblue), ': A period of time'),
		ind(ctable(phrase('A year and two days'),rarrow,time('1Y2D')
			).rjustify('c')),
#		ind(ctable(phrase('Three months'),rarrow,time('3M')).rjustify('c')),
	pause,

	ctable(_('Sequence').color(darkblue), ': A recurring range'),
		ind(ctable(phrase('Friday'),rarrow,
			'$\dots$',time('[Nov 4, Nov 5)'),',',time('[Nov 11, Nov 12)'),'$\dots$'
			).rjustify('c')),
		ind(ctable(phrase('March 13'),',',phrase('[every] hour'),
			',',phrase('May'),',$\dots$')),

nil){ |slide| slide.label('time_types').signature(21) }

################################################################################
# TIME -- types 2
################################################################################
slide!('\textbf{Time:} Primitive Types',
	'Really, there are two more types:',
	'','','',
	ctable(_('Number').color(darkblue), ': A number, kept as itself'),
		ind('(in contrast to becoming the n$^\textrm{th}$ day of the month, etc.)'),
	'','','',
	ctable(_('Nil').color(darkblue), ': A word without inherent temporal meaning'),

nil){ |slide| slide.label('time_types2').signature(0) }

################################################################################
# TIME -- ranges
################################################################################
slide!('\textbf{Time:} Ranges',
	center,
	describe(
		['Past', urange('$\infty$',-1)],
		['Future', urange('$\infty$')],
		['Yesterday', urange('1 day',-1)],
		['Tomorrow', interval(ctable(now,time('+ 1 day')),'1 day','')],
		['Today', urange('1 day')],
		['Reference', range(now,now)],
	nil),
	left,
nil){ |slide| slide.label('time_ranges').signature(20) }

################################################################################
# TIME -- durations
################################################################################
slide!('\textbf{Time:} Durations',
	center,
	describe(
		['Second',  time('1S')],
		['Minute',  time('1m')],
		['Hour',    time('1H')],
		['Day',     time('1D')],
		['Week',    time('1W')],
		['Month',   time('1M')],
		['Quarter', time('1Q')],
		['Year',    time('1Y')],
		['Decade',  time('1E')],
		['Century', time('1C')],
	nil),
	left,
nil){ |slide| slide.label('time_durations').signature(3) }

################################################################################
# TIME -- sequences
################################################################################
slide!('\textbf{Time:} Sequences',
	center,
	describe(
		['Day of Week', ctable(monday,',',tuesday,',',friday)],
		['Day of Month', ctable(dom(8),',',dom(13),',',dom(30))],
		['Month of Year', ctable(jan,',',apr,',',nov)],
		['$\dots$','',],
		['Second', sec],
		['Hour', hour],
		['$\dots$','',],
		['Century', century],
	nil),
	left,
nil){ |slide| slide.label('time_sequences').signature(10) }

################################################################################
# TIME -- as lambda
################################################################################
slide!('\textbf{Time:} Primitives As Lambdas',
	#times as functions
	center,
	describe(
		['\texttt{Duration}', 'Known a-priori'],
		['\texttt{Range}', ctable('$\lambda(\texttt{Time})$; e.g. ',phrase('Today'))],
		['\texttt{Sequence}', ctable('$\lambda(\texttt{Time},\textrm{\red{offset}})$; e.g. ',phrase('[last/this/next] Friday'))],
	nil),
	left,
	'',
	pause,

	#examples
	_('For Example:').color(darkblue),
	center,
	table(
		[ctable(time('Today'),'(',ground('Nov 10, 2011'),')'),
			rarrow, time('Nov 10, 2011')],
		pause,
		[ctable(time('Friday'),'(',ground('Nov 10, 2011'),'\red{0})'),
			rarrow, time('Nov 11, 2011')],
		[ctable(time('Friday'),'(',ground('Nov 10, 2011'),'\red{-1})'),
			rarrow, time('Nov 4, 2011')],
		[ctable(time('Friday'),'(',ground('Nov 10, 2011'),'\red{1})'),
			rarrow, time('Nov 18, 2011')],
	nil),
	left,
	pause,

	'','',
	_('Pragmatic ambiguity is usually ambiguity in \texttt{Sequence} offset').bold,


nil){ |slide| slide.label('time_primitivesfxns').signature(8) }

################################################################################
# TIME -- as distribution
################################################################################
slide!('\textbf{Time:} Sequences As Distributions',
	#main idea
	center,
	_('Define distribution over possible offsets').bold,
	left,
	'',
	pause,
	
	#characterize distribution
	_('Two Parts:').color(darkblue),
	ind('Define origin point (offset=0)'),
	ind(ind('Find closest point in enclosing container')),
	ind(ind(ctable(time('Friday'),rarrow,'\textit{Closest Friday this week}'))),
	ind('Distribution monotonically decreases from origin'),
	pause,

	center,
	phrase('Friday'),
	barGraph(fridayDist).yrange(0,1).scale(0.5),
	left,
nil){ |slide| slide.label('time_sequencedistribution').signature(17) }

################################################################################
# TIME -- distribution types
################################################################################
slide!('\textbf{Time:} Characterizing Distributions',
	#global vs local
	_('Single (global) distribution vs. Local distributions per primitive').color(darkblue),
	ind(ctable('is ',time('Friday'),' distributed the same as ',time('2$^\textrm{nd}$'),'?')),
	ind('Fine grained control, but sparsity concerns'),
	'',
	pause,

	#multinomial vs. gaussian
	_('Multinomial of Gaussian (or other?)').color(darkblue),
	ind('Gaussian more smoothed'),
	ind('Gaussian can take \textit{distance} from reference as well'),
	center,
	ctable(
		barGraph(fridayDist).yrange(0,1).scale(0.4),
		'vs.',
		image('img/gaussian.jpg').scale(0.55),
	nil).rjustify('c').cmargin(u(0.5)),
	left,

	pause,
	ind('Multinomial works better?'),
nil){ |slide| slide.label('time_characterizingdistributions').signature(10) }

################################################################################
# TIME -- functions
################################################################################
slide!('\textbf{Time:} Functions',
	center,
	_('A set of (mostly) general interval manipulation functions').bold,
	left,
	'',

	#titles
	staggeredOverlay(true,
		_('Interval (Range) Manipulation').color(darkblue).level(0),
		_('Duration Manipulation').color(darkblue).level(1),
		_('Sequence Manipulation').color(darkblue).level(2),
	nil),
	#descriptions
	center,
	staggeredOverlay(true,
		#(interval)
		describe(
			[shiftRight, 'Move a range to the left/right'],
			[shrinkEnd, 'Shrink the size of a range'],
			[catRight, 'Extend from beginning/end of a range'],
			[ctable(rangeFig(blue),'$\cap$',rangeFig(blue)), 'Intersect two ranges'],
			[cat, 'Concatenate two ranges'],
		nil).level(0),
		#(duration)
		describe(
			[time('$\approx x$'), 'Fuzzify duration'],
			[time('$n * x$'), 'Multiply duration'],
		nil).level(1),
		#(sequence)
		describe(
			[shiftLeft(nil,'1'), 'Move one forward/back in a sequence'],
		nil).level(2),
	nil).pivot(0,1),
	left,
nil){ |slide| slide.label('time_functions').signature(35) }

################################################################################
# TIME -- function application
################################################################################
slide!('\textbf{Time:} Function Application',
	#typed functions
	'Each function has a type signature',
		ind('$f(\texttt{Range},\texttt{Duration}):\texttt{Range}$'),
		ind('$f(\texttt{Sequence},\texttt{Duration}):\texttt{Sequence}$'),
		ind('$f(\texttt{Range},\texttt{Range}):\texttt{Range}$'),
		ind('$f(\texttt{Duration}):\texttt{Duration}$'),
		ind('$\dots$'),
		pause,
		ind('\textit{Arity 2 functions can behave as unaries with reference time}'),
	pause,

	#compisitional
	'Can apply functions completely compositionally',
	center,
		phrase('The week after last Friday'),
		darrow,
		shiftRight( shiftLeft(friday,'1',false), '1 week'),
	left,
nil){ |slide| slide.label('time_fnapplication').signature(14) }

################################################################################
# Parsing -- grammar
################################################################################
slide!('\textbf{Parsing:} Grammar',
	center,
	_('Can parse these function applications using their').bold,
	_('type signatures').bold,
	left,
	'','','',
	center,
	staggeredOverlay(true,
		#last friday the 13th
		lastFridayTypes,
		#last 2 days
		Parse.new(
			['\texttt{Range}',
				['$f(\texttt{Duration}):\texttt{Range}$','last'],
				['\texttt{Duration}',
					['\texttt{Number}','2'],
					['\texttt{Duration}','days'],
				],
		]).constituency,
	nil).pivot(0,0),
	left,
nil){ |slide| slide.label('parsing_grammar').signature(13) }

################################################################################
# Parsing -- synchronous grammar
################################################################################
slide!('\textbf{Parsing:} Synchronous Grammar',
	#description
	_('Parsing two things in parallel').color(darkblue),
		ind('A \textbf{parse} over temporal types'),
		ind('A \textbf{value} from repeated function applications'),
	'','',
	pause,

	#example
	_('Value is deterministic from parse').color(darkblue),
	center,
	staggeredOverlay(true,
		lastFridayTypes,
		lastFriday,
	nil).pivot(0,0),
	left,
nil){ |slide| slide.label('parsing_synchronous').signature(5) }

################################################################################
# Parsing -- ambiguity
################################################################################
slide!('\textbf{Parsing:} Ambiguity',
	'Ambiguity in parsing yields different values',
	table(
		[lastFridayTypes.scale(0.75),rarrow,time('May 13 2011')],
		pause,
		[lastFriday13Types.scale(0.75),rarrow,time('Oct 13 2010')],
	nil).rjustify('c'),
	pause,
	
	center,
	'PCFG will have an opinion about where \textit{Friday} should go',
	left,
nil){ |slide| slide.label('parsing_ambiguity').signature(6) }

################################################################################
# Parsing -- difficult ambiguity
################################################################################
slide!('\textbf{Parsing:} Difficult Ambiguity',
	_('What about these cases?').color(darkblue),
	center,
	table(
		[	
			rtable(may13,'13 $\rightarrow$ DOM(13)').rmargin(u(0.25)).cjustify('c'),
			'vs.',
			rtable(may2011,'13 $\rightarrow$ Year(2011)').rmargin(u(0.25)).cjustify('c'),
		],
		pause,
		[aWeek,'vs.',thisWeek],
	nil).rjustify('c').cjustify('c').margin(u(1.0),u(1.0)),
	left,
nil){ |slide| slide.label('parsing_hardambiguity').signature(14) }

################################################################################
# Parsing -- difficult ambiguity
################################################################################
slide!('\textbf{Parsing:} Difficult Ambiguity',
	#number ambiguity
	_('Handling \textit{May 13} vs \textit{May 2011}').color(darkblue),
	ind('Use multiple types of numbers').color(darkred),
	ind('Based on length -- 2011 is orders of magnitude larger than 13'),
	ind('Based on ordinality -- May 13$^\textrm{th}$ versus May 2011'),
	ind('\textit{(in progress)} Let the time value have a say in the probability'),
	'','','',
	pause,

	#nil ambiguity
	_('Handling \textit{a week} vs \textit{this week}').color(darkblue),
	ind('Would like to take cues from \texttt{Nil}s').color(darkred),
	ind('$P(\texttt{Nil},X \mid X) \rightarrow 
		P((\texttt{Nil},\textit{a}), X \mid X)$'),
	ind('$P(\texttt{Nil},X \mid X) \rightarrow 
		P((\texttt{Nil},\textit{this}), X \mid X)$'),
nil){ |slide| slide.label('parsing_hardambiguitysols').signature(12) }

################################################################################
# TRAINING - data
################################################################################
slide!('\textbf{Training:} Data',
	#training
	ctable(
		_('At Training: ').color(darkblue),
		'given: ',
		ctable('$\left\{\right.($','$x$',',',time('$y$'), '$)\left.\right\}$'),
	nil),
	ind(ctable(
			'Namely: ',
			'$\left\{\right.($',
			ctable('(',phrase('Phrase'),',',ground('Reference'),')'),
			',',
			time('Time'), 
			'$)\left.\right\}$',
		nil)),
	pause,
	ind('\textit{Not} given interpretation'),
	
	pause,
	ind('In general, interpretation is ambiguous from time'),
	ind(ind(ctable('( (',phrase('w1 w2'),',',ground('Nov 10 2011'),') , ',time('Nov 3 2011'), ')'))),
	pause,

	center,
	staggeredOverlay(true,
		#last thursday
		ctable(
			Parse.new(
				[shiftLeft(thursday,1), 
					[shiftLeft, 'w1'],
					[thursday, 'w2'],
				]
			).constituency,
			rarrow,
			'e.g. \textit{last Thursday}',
		nil).rjustify('c').cmargin(u(0.5)),
		#week ago
		ctable(
			Parse.new(
				[shiftLeft(now,'1 week'), 
					['1W', 'w1'],
					[shiftLeft, 'w2'],
				]
			).constituency,
			rarrow,
			'e.g. \textit{week ago}',
		nil).rjustify('c').cmargin(u(0.5)),
		#nov 3
		ctable(
			Parse.new(
				[intersect(nov,domrd(3)), 
					[nov, 'w1'],
					[domrd(3), 'w2'],
				]
			).constituency,
			rarrow,
			'e.g. \textit{November 3}',
		nil).rjustify('c').cmargin(u(0.5)),
	nil),
	left,
	pause,

	#test
	ctable(
		_('At Test: ').color(darkblue),
		'given ',
		ctable('(',phrase('Phrase'),',',ground('Reference'),')'),
		' find ',
		time('Time'), 
	nil),
	pause,
	ind(ctable('In fact, find $k$-best ',time('Time'),'s')),
nil){ |slide| slide.label('training_data').signature(24) }

################################################################################
# TRAINING - parses
################################################################################
slide!('\textbf{Training:} Parsing',
	center,
	_('EM / bootstrapping like training of latent parses').bold,
	left,
	'','',
	pause,

	_('E-Step').color(darkblue),
	ind(_('1. Find $k$-best parses using current model')),
	ind(_('2. Take parses which resolve to the correct time')),
	ind(_('3. Re-weight their probabilities to sum to $1.0$')),
	ind(_('4. Add re-weighted counts to expected sufficient statistics')),
	pause,
	center,
	'$
		ess(c_1,c_2 \mid p) = \sum\limits_{(x,y) \in \sD}
			\sum\limits_{\hat y \in \textrm{k-best}(x)}
				\frac{
					\1(\hat y = y) \textrm{count}_{\hat y}(p \rightarrow c_1,c_2) P(\hat y)
				}{
					\sum\limits_{y\' \in \textrm{k-best(x)}} \1(y\' = y) P(y\')
				}
	$',
	left,
	pause,

	_('M-Step').color(darkblue),
	ind(_('Normalize counts, with some Dirichlet smoothing')),
nil){ |slide| slide.label('training_parsing').signature(15) }

################################################################################
# TRAINING - times
################################################################################
slide!('\textbf{Training:} Times',
	_('Same Idea As Parses').color(darkblue),
	ind('Collect \textit{correct} parse trees weighted by their normalized score'),
	'','','',
	pause,

	_('E-Step').color(darkblue),
	ind('Each time was evaluated at an offset and reference'),
	pause,
	ind('\textbf{Multinomial:} Regular E-step for multinomials'),
	ind('\textbf{Gaussian:} Update normalized distance from reference'),
	'','',
	pause,

	_('M-Step').color(darkblue),
	ind('Standard M-step; Dirichlet smoothing for multinomial'),

nil){ |slide| slide.label('training_times').signature(5) }

################################################################################
# EVAL -- methods
################################################################################
slide!('\textbf{Evaluation:} Methods',
	_('Score Highest Probability Grounded Range').color(darkblue),
	ind('$P(t) = 
		P_{\textrm{PCFG}}(\textrm{parse}) * 
		P_{\textrm{Time}}(\textrm{grounded range} \mid \textrm{reference})$'),
	ind(table(
		[_('$P_{\textrm{PCFG}}$:'), 'The probability of the parse'],
		[_('$P_{\textrm{Time}}$:'), 'The probability from the time\'s distribution'],
	nil)),
	pause,
	ind('Take $\argmax P(t)$ (or, $\argmax P_{\textrm{PCFG}}$, then $\argmax P_{\textrm{Time}}$)'),
	pause,

	_('Compute Difference From Gold').color(darkblue),
	ind('Always scoring a \texttt{Duration} or (grounded) \texttt{Range}'),
	ind(table(
		pause,
		[_('\texttt{Duration}:').bold,
			'Absolute difference'],
		['',ctable(time('1D'),'-',time('1W'),rarrow,time('6D')).rjustify('c')],
		pause,
		[_('\texttt{Range}:').bold,
			'Sum Difference of Endpoints'],
		['',ctable(range('11/10','11/11'),'-',range('11/9','11/11'),rarrow,time('1D')).rjustify('c')],
	nil).cmargin(u(0.5))),
	pause,

	_('Correct if Difference is Zero').color(darkblue),

nil){ |slide| slide.label('eval_methods').signature(13) }


################################################################################
# EVAL -- datasets
################################################################################
slide!('\textbf{Evaluation:} Datasets',
	_('TempEval2 English').color(darkblue),
	ind('182 documents'),
	ind('1208 time expressions'),
	pause,

	_('New York Times').color(darkblue),
	ind('Automatically annotated with SUTime'),
	ind('$\infty$ documents'),
	pause,

	_('Combination?').color(darkblue),
	ind('Train NYT / Test NYT does well'),
	ind('Train NYT / Test TempEval2 does not'),
	pause,

	_('More Interesting Expressions').color(darkblue),
	ind('VerbMobil Meeting Scheduling \textit{(in progress)}'),
	ind('Any Ideas?').bold,
nil){ |slide| slide.label('eval_data').signature(3) }

################################################################################
# EVAL -- TempEval2
################################################################################
slide!('\textbf{Evaluation:} TempEval2 Results',
	table(
		[_('System').color(darkblue), 
				rtable('Train','Correct').cjustify('c').color(darkblue), 
				rtable('Train','Attempted').cjustify('c').color(darkblue),
				rtable('Dev','Correct').cjustify('c').color(darkblue), 
				rtable('Dev','Attempted').cjustify('c').color(darkblue)],
		['','','','',''],
		['GUTime', 0.67, 0.71, 0.68, 0.79],
		pause,
		['SUTime', '\textbf{0.75}', '\textbf{0.90}', 0.77, '\textbf{0.95}'],
		pause,
		['HeidelTime1', '?', '?', '\textbf{0.85}', 0.82],
		pause,
		['Me (good day)', 0.74, 1.0, 0.67, 1.0],
		[_('Me (normal)').color(darkblue).bold, 0.68, 1.0, 0.65, 1.0],
	nil).cmargin(u(0.5)).rmargin(u(0.4)).cjustify('lcccc').rjustify('r'),
nil){ |slide| slide.label('eval_results').signature(18) }

################################################################################
# EVAL -- Dataset Oddities I
################################################################################
slide!('\textbf{Evaluation:} Impossible Annotations',
	center,
	_('4.5\% of annotations are impossible').bold,
	left,
	'','',
	pause,
	
	_('Wrong Type').color(darkblue),
	ind(table(
		[phrase('last 24 hours'),rarrow,time('1D')],
		[phrase('a day earlier'),rarrow,time('1D')],
		[phrase('the last four months'),rarrow,time('4M')],
	nil)),
	'','','',
	pause,

	_('Fuzzy / Inconsistent').color(darkblue),
	ind(table(
		[phrase('next week'),rarrow,time('1998-WXX')],
		[phrase('a few years ago'),rarrow,time('199X')],
		pause,
		[phrase('from time to time'),rarrow,time('The Future')],
	nil)),
nil){ |slide| slide.label('eval_impossible').signature(13) }

################################################################################
# EVAL -- Dataset Oddities II
################################################################################
slide!('\textbf{Evaluation:} Impossible Annotations',
	center,
	_('4.5\% of annotations are impossible').bold,
	left,
	'','',
	
	_('Wrong Given Reference').color(darkblue),
	ind(ctable('e.g. ',phrase('Monday'),' refers to Monday too far away')),
	'','','',
	pause,

	_('Simply Wrong').color(darkblue),
	ind(table(
		[phrase('eighth day'),rarrow,time('Aug 15 1990')],
		[phrase('Monday'),rarrow,time('XXXX-WXX-1TNI')],
		[phrase('a few minutes'),rarrow,time('XM (months)')],
	nil)),
nil){ |slide| slide.label('eval_impossible2').signature(3) }

################################################################################
# EVAL -- Errors I
################################################################################
slide!('\textbf{Evaluation:} Common Errors',
	#type errors
	_('Type Errors').color(darkblue),
	ind(table(
		[phrase('week'),rarrow,
		range('Nov 7 2011', 'Nov 14 2011').scale(0.6),
		'gold: ', time('1W')],
		[phrase('the week'),rarrow,
		range('Nov 7 2011', 'Nov 14 2011').scale(0.6),
		'gold: ', time('1W')],
		nil).cmargin(u(0.5))),
	pause,
	ind('\textbf{Fix:} Lexicalize Nils better; more data'),
	'',
	pause,

	#parsing ambiguity
	_('Parsing Ambiguity').color(darkblue),
	ind(ctable(time('DOM(6)'),' intersects with ',time('1996'))),
	pause,
	ind('\textbf{Fix:} Incorporate likelihood of time'),
	'',
	pause,
	
	#reference time confusion
	_('Reference Time Confusion').color(darkblue),
	ind('If you always publish on Thursday, Thursday parses as Today'),
	pause,
	ind('\textbf{Fix:} More (even automatically generated) data'),
nil){ |slide| slide.label('eval_errors').signature(10) }

################################################################################
# EVAL -- Errors II
################################################################################
slide!('\textbf{Evaluation:} Common Errors',
	#wrong decode
	_('Wrong Vitterbi Decode').color(darkblue),
	ind(ctable(
		phrase('Monday'),rarrow,
		range('Mar 16 1998', 'Mar 17 1998').scale(0.6),
		'gold: ',range('Mar 23 1998', 'Mar 24 1998').scale(0.6)
		).cmargin(u(0.5))),
	pause,
	ind('\textbf{Fix:} WONTFIX (getting into pragmatics)'),
	'',
	pause,
	
	#grammar deficiency
	_('Grammar Deficiency').color(darkblue),
	ind(ctable(phrase('minute and a half'),rarrow,time('1m30S'))),
	pause,
	ind('\textbf{Fix:} More primitives, e.g. +'),
	pause,
	ind('But, always going to miss something'),
	
nil){ |slide| slide.label('eval_errors2').signature(1) }

################################################################################
# CONCLUSION
################################################################################
slide!('\textbf{Conclusion}',
	_('Takeaway Points').color(darkblue),
	ind('Probabilistic $\rightarrow$ handles ambiguity, propagates uncertainty'),
	ind('Compositionally parse time'),
	ind('Synchronous grammar of \texttt{Types} and \textit{Function} applications'),
	ind('EM/Bootstrapping training'),
	'',
	pause,

	_('Results').color(darkblue),
	ind('Work in progress!'),
	ind('Not hopeless numbers, with room for improvement'),
	'','','',
	pause,

	center,
	_('Thank You!').color(darkblue).scale(1.5),
	left,
	
nil){ |slide| slide.label('eval_conclusion').signature(5) }




################################################################################
# FINISH
################################################################################
finishPresentation





