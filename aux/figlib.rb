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
	def node(*args)
		def vert(*args)
			rtable(*args).center.rmargin(u(0.20))
		end
		def horiz(*args)
			ctable(*args).cmargin(u(0.25))
		end
		vert(*[args[0], horiz(*args[1..-1])])
	end
	def synedge(a,b)
		path(
			tdown(a).post{ |x| x.add(upair(0.0,0.00)) },
			tup(b).post{ |x| x.add(upair(0.0,0.05)) }
			).arrow
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

	def constituency
		def subTree(lst,probs) #[tree, root, [[edge_begin,edge_end], ...]
			if not lst.is_a? Array then
				raise "Lex mismatch: #{probs} != #{lst}" if probs and not probs == lst
				leaf = leaf(lst)
				[leaf,leaf,[]] # base case
			else
				#--Argument Parse
				raise "Too few arguments in tree #{lst}" if lst.length == 0
				#(head)
				head = head(lst[0],probs ? probs[0] : "")
				#(children -- recursive case)
				rec = (1...lst.length).map{ |i| 
					term = lst[i]
					subprobs = probs ? probs[i] : nil
					subTree(term,subprobs) 
				}
				children = rec.map{ |tuple| tuple[0] }
				edges = rec.map{ |tuple| [head,tuple[1]] }
				rec.each{ |tuple| tuple[2].each{ |e| edges << e } } # add new edges
				#--Render Return
				[
					node(*[head,*children]),
					head,
					edges
				]
			end
		end
		info = subTree(@input,@probs)
		tree = info[0]
		root = info[1]
		edges = info[2]
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
def phrase(txt); _("\\textit{#{txt}}").color(darkgreen); end
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
	nil)
end
def clock(txt,img='img/times.png')
	ctable(
		image(img).scale(0.1),
		time(txt),
	nil)
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
def day;     calendar('DAY'); end
def week;    calendar('WEEK'); end
def month;   calendar('MONTH'); end
def quarter; calendar('QUARTER'); end
def year;    calendar('YEAR'); end
def decade;  calendar('DECADE'); end
def century; calendar('CENTURY'); end

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
