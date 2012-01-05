#!/usr/bin/ruby
require 'rfig/FigureSet'
require "#{ENV['HOME']}/workspace/time/aux/figlib.rb"
require 'lib.rb'

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
				[dom(13),'13$^{\textrm{th}}$'],
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
					[dom(13),'13$^{\textrm{th}}$'],
				],
			],
		]
	).constituency
end
def lastFridayTypes
	Parse.new(
		['\texttt{Sequence}',
			['\texttt{Sequence}',
				['$f: \texttt{Sequence} \rightarrow \texttt{Sequence}$','last'],
				['\texttt{Sequence}','Friday'],
			],
			['\texttt{Sequence}',
				['\texttt{Nil}','the'],
				['\texttt{Sequence}','13$^{\textrm{th}}$'],
			],
	]).constituency
end
def lastFriday13Types
	Parse.new(
		['\texttt{Sequence}',
			['$f: \texttt{Sequence} \rightarrow \texttt{Sequence}$','last'],
			['\texttt{Sequence}',
				['\texttt{Sequence}','Friday'],
				['\texttt{Sequence}',
					['\texttt{Nil}','the'],
					['\texttt{Sequence}','13$^{\textrm{th}}$'],
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

def next2daysTypes
	Parse.new(
	  ['\texttt{Range}',
	    ['$f(\texttt{Duration}):\texttt{Range}$','next'],
	    ['\texttt{Duration}',
	      ['\texttt{Number}','2'],
	      ['\texttt{Duration}','days'],
	    ],
	]).constituency
end

def next2days
	Parse.new(
	  [catRight(nowPadded,time('  2D')),
	    [catRight(nowPadded,''),'next'],
	    [time('2D'),
	      [time('Num(2)'),'2'],
	      [time('1D'),'days'],
	    ],
	]).constituency
end

################################################################################
# FIGURES
################################################################################
initFigureSet(
	:latexHeader => IO.readlines("#{ENV['HOME']}/lib/latex/std-macros.tex") +
	                IO.readlines("#{ENV['HOME']}/workspace/time/pub/naacl2012/macros.tex")
)

################################################################################
# GRAMMAR
################################################################################
def grammar
	rtable(
		rtable(
			next2daysTypes,
			'(a)',
		nil).cjustify('c').rmargin(u(0.1)),
		rtable(
			next2days,
			'(b)',
		nil).cjustify('c').rmargin(u(0.1)),
	nil).cjustify('c').rmargin(u(0.5))
end
printObj(
	:obj => grammar.signature(26),
	:outPrefix => 'grammar'
)
	
################################################################################
# SYSTEM
################################################################################
def sys
	table(
		#(input)
		[
			_('Input (\phrase,$t$)').color(darkblue),
			ctable(
				'(',
				phrase('Last Friday the 13 th'),
				',',
				ground('May 16 2011'),
				')',
			nil).center,
		nil],
		['',darrow],
		#(parse)
		[
			rtable(
				_('Latent').color(darkblue),
				_('parse').color(darkblue),
				_('\latent').color(darkblue),
			nil).cjustify('c'),
			lastFriday.scale(0.75),
		nil],
		['',darrow],
		#(output)
		[
			_('Output \grounded').color(darkblue),
			time('May 13 2011'),
		nil],
	nil).cjustify('c').rjustify('c').rmargin(u(0.3))
end
printObj(
	:obj => sys.signature(8),
	:outPrefix => 'system'
)

finishFigureSet
