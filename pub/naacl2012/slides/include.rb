#!/usr/bin/ruby

require 'figlib.rb'
require 'rfig/FigureSet'

################################################################################
# TREES ETC
################################################################################
##########
# Macros
##########
#--Entity Types
def phrase(txt); _("\\darkgreen{\\textit{#{txt}}}"); end
def ground(time)
	if( time.is_a? Time ) then
		_("\\texttt{#{time.strftime('%Y-%m-%d')}}").color(blue)
	elsif( time.is_a? Array ) then
		ctable( *time.map{ |x| time(x) } )
	else
		_("\\texttt{#{time}}").color(blue)
	end
end
def time(time)
	if( time.is_a? Time ) then
		_("\\texttt{#{time.strftime('%Y-%m-%d')}}").color(darkred)
	elsif( time.is_a? Array ) then
		ctable( *time.map{ |x| time(x) } )
	else
		_("\\texttt{#{time}}").color(darkred)
	end
end
def now; time('$x$'); end
def w(txt); phrase(txt); end
#--Arrows
def darrow; image('img/darrow.jpg').scale(0.05); end
def uarrow; image('img/uarrow.jpg').scale(0.05); end
def uarrowLong; image('img/uarrowLong.jpg').scale(0.05); end
def larrow; image('img/larrow.jpg').scale(0.05); end
def rarrow; image('img/rarrow.jpg').scale(0.05); end

##########
# Lex
##########
#--Functions
def calendar(txt,img='img/months.png')
	ctable(
		image(img).scale(0.025),
		time(txt),
	nil).rjustify('c')
end
def mcal(txt,img='img/calendar.png')
	ctable(
		image(img).scale(0.4),
		time(txt),
	nil).rjustify('c')
end
def clock(txt,img='img/times.png')
	ctable(
		image(img).scale(0.1),
		time(txt),
	nil).rjustify('c')
end
def hourglass(txt,img='img/hourglass_dull.png')
	ctable(
		image(img).scale(0.08),
		time(txt),
	nil).rjustify('c')
end

#--DOM
def dom(n,ord); calendar("$#{n}^{\\textrm{#{ord}}}$"); end
def dom1; dom(1,'st'); end;
def dom2; dom(2,'nd'); end;
def dom3; dom(3,'rd'); end;
def dom4; dom(4,'th'); end;
def dom5; dom(5,'th'); end;
def dom6; dom(6,'th'); end;
def dom7; dom(7,'th'); end;
def dom8; dom(8,'th'); end;
def dom9; dom(9,'th'); end;
def dom10; dom(10,'th'); end;
def dom11; dom(11,'th'); end;
def dom12; dom(12,'th'); end;
def dom13; dom(13,'th'); end;
def dom14; dom(14,'th'); end;
def dom15; dom(15,'th'); end;
def dom16; dom(16,'th'); end;
def dom17; dom(17,'th'); end;
def dom18; dom(18,'th'); end;
def dom19; dom(19,'th'); end;
def dom20; dom(20,'th'); end;
def dom21; dom(21,'st'); end;
def dom22; dom(22,'nd'); end;
def dom23; dom(23,'rd'); end;
def dom24; dom(24,'th'); end;
def dom25; dom(25,'th'); end;
def dom26; dom(26,'th'); end;
def dom27; dom(27,'th'); end;
def dom28; dom(28,'th'); end;
def dom29; dom(29,'th'); end;
def dom30; dom(30,'th'); end;
def dom31; dom(31,'st'); end
#--DOW
def dow(name); calendar(name); end
def monday;    dow('MON'); end
def tuesday;   dow('TUE'); end
def wednesday; dow('WED'); end
def thursday;  dow('THU'); end
def friday;    dow('FRI'); end
def saturday;  dow('SAT'); end
def sunday;    dow('SUN'); end
#--MON
def mon(name); calendar(name); end
def jan; mon('JAN'); end
def feb; mon('FEB'); end
def mar; mon('MAR'); end
def apr; mon('APR'); end
def may; mon('MAY'); end
def jun; mon('JUN'); end
def jul; mon('JUL'); end
def aug; mon('AUG'); end
def sep; mon('SEP'); end
def oct; mon('OCT'); end
def nov; mon('NOV'); end
#--Durations
def second;  clock('SEC'); end
def minute;  clock('MIN'); end
def hour;    clock('HOUR'); end
def day;     hourglass('DAY'); end
def week;    hourglass('WEEK'); end
def month;   hourglass('MONTH'); end
def quarter; hourglass('QUARTER'); end
def year;    hourglass('YEAR'); end
def decade;  hourglass('DECADE'); end
def century; hourglass('CENTURY'); end

#--Types
def range(txt='Range'); mcal(txt); end
def duration(txt='Duration'); hourglass(txt); end
def sequence(txt='Sequence'); calendar(txt); end
def r; mcal(''); end
def d; hourglass(''); end
def s; calendar(''); end

##########
# Function Lex
##########
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

def intersect(a,b)
	ctable(a,'$\cap$',b).cjustify('c')
end

def shiftLeft(range=nil,duration="1",active=true)
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

def shiftRight(range=nil,duration="1",active=true)
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

##########
# Trees
##########

def type(name)
	_("\\texttt{#{name}}")
end
def value(*name)
	ctable(*name).rjustify('c').color(darkred)
end

def flightsToBoston(args={})
	#(arguments)
	flights = args[:flights]
	to = args[:to]
	boston = args[:boston]
	toboston = args[:toboston]
	flightstoboston = args[:flightstoboston]
	color = args[:color]
	#(objects)
	objFlights         = nil
  objTo              = nil 
  objBoston          = nil
  objToBoston        = nil
  objFlightsToBoston = nil
	#(formatting)
	def logic(v)
		_(v)
	end
	#(parse)
	parse = Parse.new(
		[logic('$\mathbf{\lambda x.\blue{flight(x)} \land \darkorange{to}(x,\purple{boston})}$').color(color ? nil : black),
			[logic('$\lambda x.\blue{flight(x)}$').color(color ? nil : black), w('flights')],
			[logic(rtable('$\lambda f.\lambda x.$','$f(x) \land to(x,boston)$').cjustify('c')),
				[logic(rtable('$\lambda y.\lambda f.\lambda x.$','$f(x) \land \darkorange{to}(x,y)$').cjustify('c')).color(color ? nil : black),
					w('to')],
				[logic('$\purple{boston}$').color(color ? nil : black), w('Boston')]
			]
		]
	)
	#(visible arcs)
	visibleEdges = {
		objFlightsToBoston => flightstoboston,
		objToBoston    => toboston,
		objBoston      => boston,
		objTo          => to,
		objFlights     => flights
	}
	#(render)
#	parse.constituency( :visibleEdges => visibleEdges )
	parse.constituency
end

#def nextWeek(args={})
#	#(arguments)
#	nxt = args[:next]
#	week = args[:week]
#	nextweek = args[:nextweek]
#	#(objects)
#	objNext            = nil
#  objWeek            = nil 
#  objNextWeek        = nil
#	#(parse)
#	parse = Parse.new(
#		[ctable('\texttt{\darkred{moveRight1}}(',week,')'),
#			['\texttt{\darkred{moveRight1$(-)$}}','\darkgreen{last}'],
#			[friday,'\darkgreen{Friday}'],
#		]
#	)
#	#(visible arcs)
#	visibleEdges = {
#		objFlightsToBoston => flightstoboston,
#		objToBoston    => toboston,
#		objBoston      => boston,
#		objTo          => to,
#		objFlights     => flights
#	}
#	#(render)
##	parse.constituency( :visibleEdges => visibleEdges )
#	parse.constituency
#end

def lastFriday_13(color=false)
	Parse.new(
		[ctable(
				_('\texttt{moveLeft1}').color(color ? blue : darkred),
				'(',
				intersect(friday.color(color ? darkorange : darkred), dom(13,'th').color(color ? purple : darkred)),
				')'),
			[_('\texttt{moveLeft1$(-)$}').color(color ? blue : darkred),'\darkgreen{last}'],
			[intersect(friday.color(color ? darkorange : darkred), dom(13,'th')),
				[friday,'\darkgreen{Friday}'],
				[dom(13,'th'),
					['\textsf{Nil$_\textsf{the}$}','\darkgreen{the}'],
					[dom(13,'th').color(color ? purple : darkred),'\darkgreen{13$^{\textrm{th}}$}'],
				],
			]
		]
#		[intersect( ctable('\texttt{\darkred{moveLeft1}}(',friday,')'), dom(13,'th') ),
#			[ctable('\texttt{\darkred{moveLeft1}}(',friday,')'),
#				['\texttt{\darkred{moveLeft1$(-)$}}','\darkgreen{last}'],
#				[friday,'\darkgreen{Friday}'],
#			],
#			[dom(13,'th'),
#				['\textsf{Nil$_\textsf{the}$}','\darkgreen{the}'],
#				[dom(13,'th'),'\darkgreen{13$^{\textrm{th}}$}'],
#			],
#		]
	).constituency
end

def last2days(args={})
	#(arguments)
	last = args[:last]
	two = args[:two]
	days = args[:days]
	twodays = args[:twodays]
	lasttwodays = args[:lasttwodays]
	change = args[:change]
	#(objects)
	objLastTwoDays = nil
  objTwoDays     = nil 
  objLast        = nil
  objTwo         = nil
  objDays        = nil
	#(parse)
	parse = Parse.new(
		[objLastTwoDays = choose(lasttwodays, type('Range'), value('\texttt{takeLeft}$($', 2, '$\times$', day, '$)$')),
			[objLast = choose(last, type('$f:$ Duration$\rightarrow$Range'), value('\texttt{takeLeft}$($--$)$')), 
				phrase('last')],
			[objTwoDays = choose(twodays, type('Duration'), value(2,'$\times$',day)),
				[objTwo = choose(two, type('Number'), value('2')), 
					change ? phrase('\textbf{3}') : phrase('2')],
				[objDays = choose(days, type('Duration'), day), 
					change ? phrase('\textbf{months}') : phrase('days')]
			]
		]
	)
	#(visible arcs)
	visibleEdges = {
		objLastTwoDays => lasttwodays,
		objTwoDays     => twodays,
		objLast        => last,
		objTwo         => two,
		objDays        => days
	}
	#(render)
	parse.constituency( :visibleEdges => visibleEdges )
end

def ambiguous(cand,gloss=['w$_1$', 'w$_2$'],overlay=true)
	fn = overlay ? lambda { |cand,*args| choose(cand,*args) } : lambda { |cand,*args| cand ? args[cand] : nil }
	#(parse)
	Parse.new(
		#(top)
		[fn.call(cand,
			value('moveRight1$($',tuesday,'$)$'),
			value('moveRight$($',now,',',week,'$)$'),
			value(tuesday),
			value('moveLeft1$($',tuesday,'$)$'),
			value('moveLeft1$($',friday,'$)$'),
			value('takeLeft$($',week,'$)$'),
			value('takeRight$($',month,'$)$'),
			value('moveRight$($',now,',',month,'$)$'),
			nil),
			#(left branch)
			[fn.call(cand,
				value('moveRight1$($\textrm{--}$)$'), 
				value('moveRight$($',now,',\textrm{--}$)$'), 
				value('Nil'), 
				value('moveLeft1$($\textrm{--}$)$'), 
				value('moveLeft1$($\textrm{--}$)$'), 
				value('takeLeft$($\textrm{--}$)$'), 
				value('takeRight$($\textrm{--}$)$'), 
				value('moveRight$($',now,',\textrm{--}$)$'), 
				nil),
				phrase(gloss[0])],
			#(right branch)
			[fn.call(cand,
				value(tuesday),
				value(week),
				value(tuesday),
				value(tuesday),
				value(friday),
				value(week),
				value(month),
				value(month),
				nil),
				phrase(gloss[1])]
		]
	).constituency
end


def ambiguousWithText(cand)
	ctable(
		#(parse)
		ambiguous(cand),
		#(interpretations)
		rtable(
			blank(cand>=0, ctable('e.g., ',phrase('w$_1=$ next, w$_2=$ Tuesday'))),
			blank(cand>=1, ctable('e.g., ',phrase('w$_1=$ next, w$_2=$ week'))),
			blank(cand>=2, ctable('e.g., ',phrase('w$_1=$ the,  w$_2=$ Tuesday'))),
		nil).rmargin(u(0.5)),
	nil).cmargin(u(1.0))
end

def kbest(probs=nil,correct=false)
	def p(prob,tree)
		rtable(tree,prob).cjustify('c').rmargin(u(0.2))
	end
	table(
		[
			p(choose(probs,0.008,'\red{0.57}'),
				ambiguous(0,['next','Tuesday'],false).scale(0.5)),
			p(choose(probs,0.005,'\red{0.36}'),
				ambiguous(1,['next','Tuesday'],false).scale(0.5)),
			p(choose(probs,soft(!correct,0.004),'\grey{0.00}'),
				soft(!correct,ambiguous(3,['next','Tuesday'],false).scale(0.5))),
			p(choose(probs,0.001,'\red{0.07}'),
				ambiguous(2,['next','Tuesday'],false).scale(0.5)),
		nil],
		[
			p(choose(probs,soft(!correct,0.0007),'\grey{0.00}'),
				soft(!correct,ambiguous(4,['next','Tuesday'],false).scale(0.5))),
			p(choose(probs,soft(!correct,0.0004),'\grey{0.00}'),
				soft(!correct,ambiguous(5,['next','Tuesday'],false).scale(0.5))),
			p(choose(probs,soft(!correct,0.0003),'\grey{0.00}'),
				soft(!correct,ambiguous(6,['next','Tuesday'],false).scale(0.5))),
			p(choose(probs,soft(!correct,0.0001),'\grey{0.00}'),
				soft(!correct,ambiguous(7,['next','Tuesday'],false).scale(0.5))),
		nil],
	nil).cmargin(u(0.25)).rmargin(u(0.5)).cjustify('c')
end

def ambiguiousLex
	[
		Parse.new([value('moveRight1$($\textrm{--}$)$'), 'next']).constituency,
		Parse.new([value('moveRight$($',now,',\textrm{--}$)$'), 'next']).constituency,
		Parse.new([value('Nil'), 'next']).constituency,
		Parse.new([value(tuesday), 'Tuesday']).constituency,
		Parse.new([value(week), 'Tuesday']).constituency,
		Parse.new([value(tuesday), 'Tuesday']).constituency,
	]
end

def ambiguiousGrammar
	[
		Parse.new(
			[value(s), 
				value(s,'$\rightarrow$',s),
				value(s)
			]).constituency,
		Parse.new(
			[value(r), 
				value(d,'$\rightarrow$',r),
				value(d)
			]).constituency,
		Parse.new(
			[value(s), 
				value('Nil'),
				value(s)
			]).constituency,
	]
end

################################################################################
# STRUCTURED SLIDES
################################################################################
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
		text = @detected ? 
			overlay(no(txt)[0].color(nocolor), w(txt).bold) :
			overlay(w(txt).bold.color(nocolor), no(txt)[0])
		v = nil
		t = rtable(
			#(text)
			text,
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
		no('-- there\'s still'),
		yes('a few days','$\sim$1D', '$\sim$1D'),
	]
	#--Figure
	#(stationary)
	stationary = rtable(
		ctable(*data.map{ |t,v,g| t }),
		(if(grounded) then
			ctable(
				ctable(_('['), ground(Time.mktime(2012,6,2)), _(']')), 
				*data.map{ |t,v,g| g }).cmargin(u(0.5))
		else
			ctable(
				ctable(_('[').color(nocolor), ground(Time.mktime(2012,6,2)).color(nocolor), _(']').color(nocolor)), 
				*data.map{ |t,v,g| g ? g.color(nocolor) : g }).cmargin(u(0.5))
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
				choose(input,phrase('phrase'),phrase('Last Friday the 13th')),
				blank(input,','),
				choose(input,ground('reference'),ground(Time.mktime(2012,6,5))),
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
			choose(output, time('normalized time'), time(Time.mktime(2012,1,13))),
		nil],
	nil).cjustify('c').rjustify('c').rmargin(u(0.3)).cmargin(u(0.5))
end

def comparison(l=0,t=-1,colorLogic=false,colorTime=false)
	def flight(v); time(v); end
	table(
		[
			blank(l>1,flight('Delta 3871').color(black).bold),
			blank(t>1,time(Time.mktime(2012, 1, 13)).bold),
		],
		[
			blank(l>0,centeredOverlay(uarrowLong,image('img/database.png').scale(0.15))),
			blank(t>0,centeredOverlay(uarrowLong,circle(u(0.25)).color(white).fill,ground(Time.mktime(2012,6,5)))),
		],
		[
			blank(l>=0,flightsToBoston({:color => colorLogic}).scale(0.75)),
			blank(t>=0,lastFriday_13(colorTime).scale(0.75)),
		],
	nil).cmargin(u(0.5)).rmargin(u(0.25)).cjustify('cc')
end

def relatedParsing(state=nil)
	rtable(
		ind(ind(choose(state,
			'\darkred{Zelle \& Mooney (1996), Zettlemoyer \& Collins (2005/2007)}',
			'Zelle \& Mooney (1996), Zettlemoyer \& Collins (2005/2007)',
			'Zelle \& Mooney (1996), Zettlemoyer \& Collins (2005/2007)'
		))), 
		ind(ind(ctable(
			choose(state,'\darkred{Kate et al. (2005)},', 'Kate et al. (2005),', 'Kate et al. (2005),'),
			choose(state,
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
			choose(state, 'Str\"{o}tgen and Gertz (2010),', '\darkred{Str\"{o}tgen and Gertz (2010),}')))),
		ind(ind(choose(state, 'Chang and Manning (2012)', '\darkred{Chang and Manning (2012)}'))),
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

def emHeaders(headerNum)
	rtable(
		(if(headerNum == 0) then
			h1('Step 1: Get $k$-best parses for phrase')
		elsif(headerNum > 0)
			h1Grey('Step 1: Get $k$-best parses for phrase')
		end),
		(if(headerNum == 1) then
			h1('Step 2: Filter and reweight correct parses')
		elsif(headerNum > 1)
			h1Grey('Step 2: Filter and reweight correct parses')
		else
			h1('')
		end),
		(if(headerNum == 2) then
			h1('Step 3: Update expected sufficient statistics')
		elsif(headerNum > 2)
			h1Grey('Step 3: Update expected sufficient statistics')
		else
			h1('')
		end),
	nil)
end

def results(trn=true,sys=[])
	def mktable(sys,nums)
		table(
		 ['\darkred{System}' ,'\darkred{Type}','\darkred{Value}'],
		 blank(sys.member?('gutime'),      ['\sys{GUTime}'     ,nums[0],nums[1]]),
		 blank(sys.member?('sutime'),      ['\sys{SUTime}'     ,nums[2],nums[3]]),
		 blank(sys.member?('heideltime'),  ['\sys{HeidelTime}' ,nums[4],nums[5]]),
		 blank(sys.member?('parsingtime'), ['\sys{ParsingTime}',nums[6],nums[7]]),
		nil).rmargin(u(0.2)).cmargin(u(0.5)).cjustify('lcc')
	end
	train = mktable(sys,['0.80','0.42','\textbf{0.94}','0.71','0.85','0.71','0.88','\textbf{0.72}'])
	test = mktable(sys,['0.72','0.46','0.85','0.69','0.80','0.67','\textbf{0.90}','\textbf{0.72}'])
	trn ? train : test
end
