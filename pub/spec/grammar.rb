#!/usr/bin/ruby

require '/home/gabor/workspace/time/aux/figlib.rb'

initFigureSet(
	:outPrefix => '.', 
	:defaultFont => 'sans-serif',
	:latexHeader => IO.readlines('/home/gabor/lib/latex/std-macros.tex') +
	['\usepackage{color}'],
	:lazy => true)

#-------------------------------------------------------------------------------
WORD = "w"
SEC = "1s"
MIN = "1m"
HR  = "1h"
DAY = "1D"
WK = "1W"
MON = "1M"
YR  = "1Y"
NOW = "REF"
def grammar
	def parse(arg); Parse.new(arg).constituency; end
	table(
		[
			_("Rule [Type]").color(blue), 
			_("Instance[s]").color(blue),
			_("Example").color(blue),
		nil],
		["Duration Introductions",
			ctable(
				parse("(#{SEC} #{WORD})"),
				parse("(#{MIN} #{WORD})"),
				parse("(#{HR} #{WORD})"),
				parse("(#{DAY} #{WORD})"),
				parse("(#{WK} #{WORD})"),
				parse("(#{MON} #{WORD})"),
				parse("(#{YR} #{WORD})"),
			nil),
			parse("(#{YR} year)"),
		nil],
		["Sequence Introductions",
			ctable(
				parse("(Mon #{WORD})"),
				'$\dots$',
				parse("(Sun #{WORD})"),
				parse("(Jan #{WORD})"),
				'$\dots$',
				parse("(Dec #{WORD})"),
			nil),
			parse("(#{YR} year)"),
		nil],
		["Special Introductions",
			ctable(
				parse("(#{NOW} now)"),
				parse("(nil #{WORD})"),
			nil),
			parse("(nil a)"),
		nil],
		["Duration Mod", 
			ctable(
				parse("(range ($f_d$ $f_{r,d}$ range) duration)"),
				parse("(range  duration ($f_d$ range $f_{r,d}$))"),
				'$\dots$',
			nil).cmargin(u(0.2)),
			parse("
				( x,x+#{WK} 
					(#{WK} week) 
					(shiftRight$_{d}$
						(shiftRight$_{r,d}$ from)
						(x,x (x now))
					)
				)"),
		nil],
		["Range Mod", 
			ctable(
				parse("(range ($f_r$ $f_{r,r}$ range) range)"),
				parse("(range  range ($f_r$ range $f_{r,r}$))"),
				'$\dots$',
			nil).cmargin(u(0.2)),
			parse("
				( 2011/05/01,2011/06/01
					(May may) 
					(cons$_{r}$
						(cons$_{r,r}$ of)
						(2011/01/01,2012/01/01 2011)
					)
				)"),
		nil],
		["Type Raises", 
			ctable(
				parse("(time,time time)"),
				parse("($f_{d}$ $f_{r,d}$)"),
				parse("($f_{r}$ $f_{r,r}$)"),
				parse("(cons$_r$ range)"),
			nil).cmargin(u(0.2)),
			parse("(May may)"),
		nil],
		["Sequence Grounding", 
			ctable(
				parse("(range duration)"),
			nil).cmargin(u(0.2)),
			parse(['$\{x \mid x \in Fri\}$', 'friday']),
		nil],
	nil).border(0.5).cjustify('cc').rjustify('c').cmargin(u(0.5)).rmargin(u(0.5))
end
printObj(
	:obj => grammar.signature(49), 
	:outPrefix => 'grammar')
#-------------------------------------------------------------------------------
def test
	Parse.new("(S (NP (DT the) (NN dog)) (VP (VB is) (NP (JJ red))))").
		constituency
end
printObj(
	:obj => test.signature(1), 
	:outPrefix => 'test')
#-------------------------------------------------------------------------------
finishFigureSet




