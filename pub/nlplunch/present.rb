#!/usr/bin/ruby

require 'rfig/Presentation'
require "#{ENV['HOME']}/workspace/time/aux/figlib.rb"

#--Set Slide Style
slideStyle = nil
slideStyle = SlideStyle.new.border(1).borderColor(black)

#--Init Presentation
initPresentation(
	:latexHeader => IO.readlines("#{ENV['HOME']}/lib/latex/std-macros.tex"),
	:edition => 'present',
	:fontSize => 32
).slideStyle(slideStyle)

def phrase(txt); _("\\textit{#{txt}}").color(darkgreen); end
def ground(time); _("\\texttt{#{time}}").color(blue); end
def time(time); _("\\texttt{#{time}}").color(magenta); end

################################################################################
# LIB
################################################################################
def range(color=black,textVal=nil)
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

def shiftLeft(range=nil,duration="")
	overlay(
		t = ctable(
			l=(range.is_a?(String) or not range) ? range(blue,range) : range,
			_('$\dots$').scale(0.5),
			r=range(grey,nil),
		nil).center,
		p = path(
			tup(r),
			tup(t).post{ |x| x.add(upair(0.0,0.2)) },
			tup(l),
		nil).arrow.curved.dashed('evenly').color(grey),
		shift( tup(p).post{ |x| x.add(upair(-0.2,0.07)) } ),
		ctable(
			_(duration).scale(0.5).color(blue),
		nil).opaque,
	nil)
end

def shiftRight(range=nil,duration="",useNow=false)
	overlay(
		t = ctable(
			r=(useNow ? _('{\tt now}').scale(0.7).color(blue) : range(grey,nil)),
			_('$~~~$').scale(0.5),
			l=(range.is_a?(String) or not range) ? range(blue,range) : range,
		nil).center,
		p = path(
			tup(r),
			tup(t).post{ |x| x.add(upair(0.0,0.2)) },
			tup(l),
		nil).arrow.curved.dashed('evenly').color(grey),
		shift( tup(p).post{ |x| x.add(upair(-0.2,0.07)) } ),
		ctable(
			_(duration).scale(0.5).color(blue),
		nil).opaque,
	nil)
end

def shrinkEnd(range=nil,duration=nil)
	overlay(
		tbl = ctable(
			farL=_('$\left[\right.$').color(grey),
			dots=_('$\dots$').scale(0.5),
			l=_('$\left[\right.$').color(blue),
			t = duration ? _(duration).color(blue) : nil,
			ctable(
				r=_('$\left.\right)$').color(blue),
				farR=_('$\left.\right)$').color(grey),
			nil).cmargin(u(-0.1)),
		nil).center,
		path(tcenter(l),t ? tleft(t) : tcenter(r)).thickness(1).color(blue),
		t ? path(tright(t),tcenter(r)).thickness(1).color(blue) : nil,
		p = path(
			tup(dots).post{ |x| x.add(upair(0.0,0.1)) },
			tup(tbl).post{ |x| x.add(upair(0.0,0.2)) },
			t ? tup(t) : tup(l).post{ |x| x.add(upair(0.1,0.0)) },
		nil).arrow.curved.dashed('evenly').color(grey),
	nil)
end

def calendar(txt,img)
	ctable(
		image(img).scale(0.03),
		time(txt),
	nil)
end
def dom(n); calendar("$#{n}^{\\textrm{th}}$",'img/calendar.png'); end
def friday; calendar('FRI','img/friday.png'); end
	
def darrow; image('img/darrow.jpg').scale(0.05); end
def uarrow; image('img/uarrow.jpg').scale(0.05); end
def larrow; image('img/larrow.jpg').scale(0.05); end
def rarrow; image('img/rarrow.jpg').scale(0.05); end
################################################################################
# FIGS
################################################################################
def lastFriday
	Parse.new(
		[ctable( shiftLeft(friday,'1'), '$\cap$', dom(13) ).rjustify('c'),
			[shiftLeft(friday,'1'),
				[shiftLeft(range(blue),'1'),'last'],
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

################################################################################
# INTRO
################################################################################
#TODO

################################################################################
# OUTLINE 
################################################################################
slide!('Outline',
	'Task Description',
	'My Theory of Time',
	'Parsing Methods',
	'Data',
	'Preliminary Results',
nil){ |slide| slide.label('outline').signature(1) }

################################################################################
# TASK -- overview
################################################################################
slide!('\textbf{Task:} Overview',
	'',
	#task
	center,
	_('Resolve temporal pharses into their \textit{grounded} time,').bold,
	_('given a reference time').bold,
	left,
	'',
	'',
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
# TASK -- training
################################################################################

slide!('\textbf{Task:} Approach',
	'',
	#task
	center,
	_('In reality, each parse has a latent parse').bold,
	left,
	'',
	'',
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
nil){ |slide| slide.label('task_approach').signature(39) }

################################################################################
# APPROACH -- prior work
################################################################################
slide!('\textbf{Approach:} Prior Work',
	_('Rule Based Systems').color(darkblue),
		ind('Benchmark to compare against').color(darkgreen),
		ind('e.g. GUTime, SUTime'),
		ind('\texttt{http://nlp.stanford.edu:8080/sutime/}'),
	'','','',
	_('Parsing to Logic').color(darkblue), 
		ind('Inspirations for approach').color(darkgreen),
		ind('CCG: (Zettlemoyer and Colins 2005/2007)'),
		ind('Dependency-based Compositional Semantics: (Liang et al 2011)'),
nil){ |slide| slide.label('approach_rulebased').signature(4) }

################################################################################
# APPROACH -- advantages
################################################################################
slide!('\textbf{Approach:} Advantages (claimed)',
#	center,'\textit{(more on each of these later)}',left,

	_('Handle [Parsing] Ambiguity').color(darkblue),
	ind('Like regular parsing, a phrase can be syntactically ambiguous'),
	ind(ctable(phrase('Last Friday the 13 th'), rarrow, time('May 13 2011'),'?')),
	ind(ctable(phrase('Last Friday the 13 th'), rarrow, time('Aug 13 2010'),'?')),

	_('Handle [Pragmatic] Ambiguity').color(darkblue),
	ind('Often it is not clear what a time a temporal notion refers to'),
	ind(ctable(phrase('Next Friday'), rarrow, time('Nov 11 2011'),'?')),
	ind(ctable(phrase('Next Friday'), rarrow, time('Nov 18 2011'),'?')),
	
	_('Propagates Uncertainty').color(darkblue),

	_('Grammar is Language Agnostic').color(darkblue),
	ind(ctable(
		phrase('Next Friday'), 'and', phrase('Viernes pr\`{o}ximo'),
		'use same time constructs',
		nil)),
nil){ |slide| slide.label('approach_advantages').signature(8) }

################################################################################
# TIME -- theory
################################################################################
slide!('\textbf{Time:} Theory',
	center,
	_('Temporal expressions can be represented by a finite set of').bold,
	_('functions applied to a finite set of primitives').bold,
	left,
	
	'','','',

	ctable(
		lastFriday.scale(0.75),
		rtable(
			_('Fuctions:').color(darkblue),
			table(
				[shiftLeft(range(blue),'1').scale(0.75),
					'Move back in the sequence'],
				['$\cap$',
					'Take overlap of two times'],
			nil).cmargin(u(0.3)),
			'','','',
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
# FINISH
################################################################################
finishPresentation





