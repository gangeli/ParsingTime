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
	:fontSize => 27
).slideStyle(slideStyle)

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

#################################################################################
## The Last Quarter of Three Years Ago
#################################################################################
#def lqotya
#	Parse.new(
#		[shiftLeft(shrinkEnd(nil,"1Q"),"3Y"),
#			[shrinkEnd(nil,"1Q"),
#				[shrinkEnd, 'last'],
#				[_("1Q").color(blue),
#					[_("1Q").color(blue), 'quarter'] ,
#					['nil', 'of'] ] ],
#			[shiftLeft("1Y","3Y"),
#				[_('3Y').color(blue), 
#					[_('3').color(blue), '3'],
#					[_('1Y').color(blue), 'years']],
#				[shiftLeft(nil,""), 'ago']] ]
#	).constituency
#end
#printObj(
#	:obj => lqotya,
#	:outPrefix => 'hello')


################################################################################
# TASK
################################################################################
slide!('Task',
	'',
nil){ |slide| slide.label('datum"""+id+"""').signature(0).titleSpacing(u(0)) }



gabor, can you make change the 1 slide you made for david into 2 slides,
      both of which are a little clearer than the current 1 slide:
        SLIDE 1:  the task:  show a picture of the "input": some sentences  and timex for each, i.e.
           SENTENCE 1    TIME 1
           SNETENCE 2     TIME 2
           SENTENCE 3     TIME 3
           then a picture of the output, a black box that takes
                SENTENC 4:   
       and produces
                TIME 4:
         SLIDE 2:  The algorithm: 
                    -   2 bullets abouto the algorithm in plain english
                    -   improve the parse tree by making one where the semantics is much more clearly compositional;
                          i don't think they'll follow the current example.

################################################################################
# FINISH
################################################################################
finishPresentation





