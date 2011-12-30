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

################################################################################
# FIGURES
################################################################################
initFigureSet

################################################################################
# GRAMMAR
################################################################################
def grammar
	table(
		[lastFriday13Types,lastFriday13],
		['(a)','(b)'],
	nil).cjustify('c').cmargin(u(0.3)).rmargin(u(0.5))
end
printObj(
	:obj => grammar.signature(11),
	:outPrefix => 'grammar'
)
	
################################################################################
# SYSTEM
################################################################################
def sys
	rtable(
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
	nil).cjustify('c').rmargin(u(0.3))
end
printObj(
	:obj => sys.signature(3),
	:outPrefix => 'system'
)

finishFigureSet
