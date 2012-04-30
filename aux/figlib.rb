#!/usr/bin/ruby

require 'rfig/FigureSet'
require "#{ENV['HOME']}/lib/ruby/earley.rb"

################################################################################
# UTILITIES
################################################################################
def stdemail(name)
	if(name.to_s == "Gabor Angeli") then
		'angeli@stanford.edu'
	elsif(name.to_s == "Christopher Manning") then
		'manning@stanford.edu'
	elsif(name.to_s == "Dan Jurafsky") then
		'jurafsky@stanford.edu'
	end
end
def author(name, email=stdemail(name))
	rtable(
		_(name).bold,
#		"\\textit{Stanford}",
		_(email),
	nil).cjustify('c').scale(0.9)
end

def describe(*rows)
	table(*rows.map{ |name,desc|
		name ? [_(name).color(darkblue), desc] : nil
	}).cmargin(u(1.0)).rmargin(u(0.2)).rjustify('c')
end

def blank(cond, txt)
	if(cond) then
		_(txt)
	else
		_(txt).color(white)
	end
end

def move(num, *args)
	elems = []
	args.each_with_index{ |term,i|
		if(i != num and term) then
			elems << _(term).color(white)
		end
	}
	elems << (num ? args[num] : nil)
	centeredOverlay(*elems)
end

def heat(value)
	Value.color(1.0, 1.0-value, 0.0)

end

################################################################################
# PARSE CLASS
################################################################################
class Parse
	@@parser = Earley::Parser.new(Earley::TREE)
	private
	#-----
	# RFIG UTIL
	#-----
	def head(term,prob)
		if prob then
			rtable(
				term,
				scale(0.5),
				_("\\textit{#{prob}}").color(grey),
			nil).center.rmargin(u(0.0))
		else
			_(term)
		end
	end
	def leaf(a)
		_("\\textit{#{a}}")
	end
	def node(rspace,cspace,thickness,*args)
		def vert(*args)
			rtable(*args).center.rmargin(u(0.40))
		end
		def horiz(*args)
			ctable(*args).cmargin(u(0.40))
		end
		vert(*[args[0], horiz(*args[1..-1])])
	end
	def synedge(a,b)
		if(a.getColor == white) then
			puts "-----_HERE"
		end
		path(
			tdown(a).post{ |x| x.add(upair(0.0,0.00)) },
			tup(b).post{ |x| x.add(upair(0.0,0.10)) }
			).arrow.thickness(2)
	end

	public
	#-----
	# MODIFY
	#-----
	def probabilities(input)
		if input.is_a? Array
			@probs = input
		else
			@probs = @@parser.parse(input)
		end
		self
	end
	#-----
	# RENDER
	#-----

	def constituency(args={})
		#--Arguments
		rspace = args[:rspace] ? args[:rspace] : 0.4
		cspace = args[:cspace] ? args[:cspace] : 0.4
		thickness = args[:thickness] ? args[:thickness] : 2
		visibleEdges = args[:visibleEdges]
		#--Recursive Function
		def subTree(lst,probs,rspace,cspace,thickness,visibleEdges) 
				#return [isVisible, tree, root, [[edge_begin,edge_end], ...]
			if not lst.is_a? Array then
				raise "Lex mismatch: #{probs} != #{lst}" if probs and not probs == lst
				leaf = leaf(lst)
				[true,leaf,leaf,[]] # base case
			else
				raise "Too few arguments in tree #{lst}" if lst.length == 0
				#(head)
				head = head(lst[0],probs ? probs[0] : "")
				#(children -- recursive case)
				rec = (1...lst.length).map{ |i| 
					term = lst[i]
					subprobs = probs ? probs[i] : nil
					subTree(term,subprobs,rspace,cspace,thickness,visibleEdges) 
				}
				#(determine if visible)
				isVisible = true
				if(visibleEdges)
					isVisible = (rec.all?{ |tuple| tuple[0] } and visibleEdges[lst[0]])
				end
				#(create recursive info)
				children = rec.map{ |tuple| tuple[1] }
				edges = isVisible ? rec.map{ |tuple| [head,tuple[2]] } : [] # add these edges
				rec.each{ |tuple| tuple[3].each{ |e| edges << e } } # add recursive edges
				#(return)
				[
					isVisible,
					node(rspace,cspace,thickness,*[head,*children]),
					head,
					edges
				]
			end
		end
		#--Start And Render
		info = subTree(@input,@probs,rspace,cspace,thickness,visibleEdges)
		isVisible = info[0]
		tree = info[1]
		root = info[2]
		edges = info[3]
		overlay(tree, *edges.map{ |pair| synedge(*pair) })
	end

	#-----
	# OVERHEAD
	#-----
	def initialize(input)
		if input.is_a? Array
			@input = input
		else
			@input = @@parser.parse(input)
		end
	end
	def to_s
		def prettyPrint(data)
			if data.is_a? Array then
				"(" + data.map{ |x| prettyPrint(x) }.join(" ") + ")"
			else
				data.gsub(/\(/,'\\(').gsub(/\)/,'\\)').gsub(/ /,'\\ ')
			end
		end
		prettyPrint(@input)
	end
end

################################################################################
# FIGURES
################################################################################

##########
# Macros
##########
#--Entity Types
def phrase(txt); _("\\darkgreen{\\textit{#{txt}}}"); end
def ground(time); _("\\texttt{#{time}}").color(blue); end
def time(time); _("\\texttt{#{time}}").color(darkred); end
def now; time('$x$'); end
def w(txt); phrase(txt); end
#--Time Constructs
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
#--Arrows
def darrow; image('img/darrow.jpg').scale(0.05); end
def uarrow; image('img/uarrow.jpg').scale(0.05); end
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
def hourglass(txt,img='img/hourglass.png')
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

def lastFriday_13
	Parse.new(
		[intersect( ctable('\texttt{\darkred{moveLeft1}}(',friday,')'), dom(13,'th') ),
			[ctable('\texttt{\darkred{moveLeft1}}(',friday,')'),
				['\texttt{\darkred{moveLeft1$(-)$}}','\darkgreen{last}'],
				[friday,'\darkgreen{Friday}'],
			],
			[dom(13,'th'),
				['\textsf{Nil$_\textsf{the}$}','\darkgreen{the}'],
				[dom(13,'th'),'\darkgreen{13$^{\textrm{th}}$}'],
			],
		]
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
		[objLastTwoDays = move(lasttwodays, type('Range'), value('\texttt{takeLeft}$($', 2, '$\times$', day, '$)$')),
			[objLast = move(last, type('$f:$ Duration$\rightarrow$Range'), value('\texttt{takeLeft}$($--$)$')), 
				phrase('last')],
			[objTwoDays = move(twodays, type('Duration'), value(2,'$\times$',day)),
				[objTwo = move(two, type('Number'), value('2')), 
					change ? phrase('\textbf{3}') : phrase('2')],
				[objDays = move(days, type('Duration'), day), 
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
