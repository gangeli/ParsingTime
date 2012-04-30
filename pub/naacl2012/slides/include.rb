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

def h1(txt); _(txt).bold.color(darkred); end
def h1Grey(txt); _(txt).bold.color(gray); end
def h2(txt); ind("\\darkblue{#{txt}}"); end


def sys(input=0,output=false,latent=false,latentParse=false)
	inputTxt = '\darkblue{Input (\darkgreen{\textbf{\phrase}},\blue{\textbf{$t$}})}'
	table(
		#(input)
		[
			inputTxt,
			ctable(
				blank(input,'('),
				move(input,phrase('phrase'),phrase('Last Friday the 13th')),
				blank(input,','),
				move(input,ground('reference'),ground('May 16 2011')),
				blank(input,')'),
			nil).center,
		nil],
		[blank(latent,darrow),''],
		#(parse)
		[
			rtable(
				blank(latent, _('Latent').color(darkred)),
				blank(latent, _('parse').color(darkred)),
				blank(latent, _('\latent').color(darkred)),
			nil).cjustify('c'),
			blank(latentParse, lastFriday_13.scale(0.75)),
		nil],
		[blank(latent,darrow),''],
		#(output)
		[
			blank(output, '\darkblue{Output \darkred{\textbf{\grounded}}}'),
			move(output, time('normalized time'), time('May 13 2011')),
		nil],
	nil).cjustify('c').rjustify('c').rmargin(u(0.3)).cmargin(u(0.5))
end

def relatedParsing(state=nil)
	rtable(
		ind(ind(move(state,
			'\darkred{Zelle \& Mooney (1996), Zettlemoyer \& Collins (2005/2007)}',
			'Zelle \& Mooney (1996), Zettlemoyer \& Collins (2005/2007)',
			'Zelle \& Mooney (1996), Zettlemoyer \& Collins (2005/2007)'
		))), 
		ind(ind(ctable(
			move(state,'\darkred{Kate et al. (2005)},', 'Kate et al. (2005),', 'Kate et al. (2005),'),
			move(state,
				'Clarke et al. (2010), Liang et al. (2011)',
				'\darkred{Clarke et al. (2010), Liang et al. (2011)}',
				'Clarke et al. (2010), Liang et al. (2011)'),
		nil)))
	)
end
def relatedSemantics(state=nil)
	rtable(
		ind(ind('Mani \& Wilson (2000), Saquete et al. (2003), Puscasu (2004)')),
		ind(ind(ctable(
			'Grover et al. (2010),',
			move(state, 'Str\"{o}tgen and Gertz (2010),', '\darkred{Str\"{o}tgen and Gertz (2010),}')))),
		ind(ind(move(state, 'Chang and Manning (2012)', '\darkred{Chang and Manning (2012)}'))),
	nil)
end

def nextFridayDistribution(show=[],hilight=[],probs=false)
	table(
		[blank(probs,_('0.04').color(heat(0.08))), blank(show.member?(1), time(hilight.member?(1) ? '\textbf{June 02, 2012?}' : 'June 02, 2012' ))],
		[blank(probs,_('0.24').color(heat(0.48))), blank(show.member?(2), time(hilight.member?(2) ? '\textbf{June 08, 2012?}' : 'June 08, 2012' ))],
		[blank(probs,_('0.43').color(heat(0.86))), blank(show.member?(3), time(hilight.member?(3) ? '\textbf{June 15, 2012?}' : 'June 15, 2012'))],
		[blank(probs,_('0.17').color(heat(0.34))), blank(show.member?(4), time(hilight.member?(4) ? '\textbf{June 22, 2012?}' : 'June 22, 2012'))],
	nil).cmargin(u(0.3))
end
