#!/usr/bin/ruby

require 'rfig/FigureSet'
require "#{ENV['HOME']}/workspace/time/aux/figlib.rb"

initFigureSet
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

################################################################################
# The Last Quarter of Three Years Ago
################################################################################
def lqotya
	Parse.new(
		[shiftRight(range(blue),'2D',true),
			[_('2D').color(blue),
				[_('2').color(blue), '2'],
				[_('1D').color(blue), 'days'] ],
			[shiftRight, 'from'],
			[_('{\tt now}').scale(0.7).color(blue), 'now~']]
	).constituency
end
printObj(
	:obj => lqotya,
	:outPrefix => 'hello')

finishFigureSet
