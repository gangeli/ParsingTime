#!/usr/bin/ruby

require 'figlib.rb'
require 'rfig/FigureSet'

def pastelred; Value.color(140.0 / 255, 84.0 / 255, 84.0 / 255); end

def outline(item=nil)
	sections = [
		'Motivation',
		'Representation',
		'Learning',
		'Results'
	]
	rtable( *sections.map_with_index{ |sec,i|
		if(i == item) then
			_(sec).bold.color(darkred)
		else
			_(sec).color(darkblue)
		end
	}).rmargin(u(1.0))
end

def example(detected=false, interpreted=nil, grounded=false, ambiguity=false)
	#--Variables
	@detected = detected
	@interpreted = interpreted
	#--Static Info
	def no(txt)
		[_("\\textit{#{txt}}")]
	end
	def yes(txt, value, ground, ambiguity=false)
		text = @detected ? [w(txt).bold] : no(txt)
		v = nil
		t = rtable(
			#(text)
			text[0],
			#(interpretation)
			@interpreted ? downarrow.thickness(3).color(pastelred) : nil,
			v = (if(@interpreted == nil)
				nil
			elsif(@interpreted == 0)
				time("?")
			else
				time(value)
			end),
		nil).cjustify('c')
		[t,v, ambiguity ? time(ambiguity).color(red) : time(ground)]
	end
	#--Data
	data = [
		no('Don\'t worry,'),
		yes('June 5', '6/5', '6/05/12'),
		no('is'),
		yes('next week','6/3-6/9', '6/03/12-6/09/12', ambiguity ? '6/10/12-6/16/12' : false),
		no('-- still'),
		yes('days','$\sim$1D', '$\sim$1D'),
		no('away')
	]
	#--Figure
	#(stationary)
	stationary = rtable(
		ctable(*data.map{ |t,v,g| t }),
		(if(grounded) then
			ctable(
				ctable(_('['), ground('June 2, 2012'), _(']')), 
				*data.map{ |t,v,g| g }).cmargin(u(0.5))
		else
			ctable(
				ctable(_('[').color(white), ground('June 2, 2012').color(white), _(']').color(white)), 
				*data.map{ |t,v,g| g ? g.color(white) : g }).cmargin(u(0.5))
		end),
	nil).rmargin(u(1.0)).cjustify('c')
	#(paths)
	overlay(
		stationary,
		*data.map{ |t,v,g|
			if(grounded and v and g) then
				path(
					tdown(v).post{ |x| x.add(upair(0.0 ,-0.1)) },
					tdown(v).post{ |x| x.add(upair(0.0 ,-0.12)) },
					tup(g).post{   |x| x.add(upair(0.0 ,0.12)) },
					tup(g).post{   |x| x.add(upair(0.0 ,0.1)) },
				nil).arrow.curved.thickness(3).color(pastelred)
			else
				nil
			end
		}
	)
end

def defn(header, txt)
	ctable(_(header).color(darkred).bold, txt).cmargin(u(0.2))
end
