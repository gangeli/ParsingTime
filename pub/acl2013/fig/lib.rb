def phrase(txt); _("\\textit{#{txt}}").color(darkgreen); end
def ground(time); _("\\texttt{#{time}}").color(blue); end
def time(time); _("\\texttt{#{time}}").color(darkred); end
def now; time('$t$'); end
def nowPadded; time('  $t$  '); end
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
#def range(start,finish)
#	a = (start.is_a?(String) ? time(start) : start)
#	b = (finish.is_a?(String) ? time(finish) : finish)
#	ctable('[',a,',',b,')').rjustify('c')
#end
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

def cat
	ctable(
	overlay(
		ctable(
			l=_('$\left[\right.$').color(blue),
			r=_('$\left.\right)$').color(darkgrey),
		nil).center,
		path(tcenter(l),tcenter(r)).thickness(1).color(blue),
	nil),
	overlay(
		ctable(
			l=_('$\left[\right.$').color(darkgrey),
			r=_('$\left.\right)$').color(blue),
		nil).center,
		path(tcenter(l),tcenter(r)).thickness(1).color(blue),
	nil),
	nil)
end

def intersect(a,b)
	ctable(a,'$\cap$',b).cjustify('c')
end

def shiftLeft(range=nil,duration="",active=true)
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

def shiftRight(range=nil,duration="",active=true)
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

def catRight(range=nil,duration="")
	overlay(
		t = ctable(
			r=(range.is_a?(String) or not range) ? rangeFig(darkgrey,range) : range,
			l=rangeFig(blue,nil),
		nil).cmargin(u(-0.05)).center,
		p = path(
			tup(r),
			tup(t).post{ |x| x.add(upair(0.0,0.2)) },
			tup(l),
		nil).arrow.curved.dashed('evenly').color(darkgrey),
		shift( tup(p).post{ |x| x.add(upair(-0.2,0.07)) } ),
		ctable(
			_(duration).scale(0.5),
		nil).opaque,
	nil)
end

def shrinkEnd(range=nil,duration=nil)
	overlay(
		tbl = ctable(
			farL=_('$\left[\right.$').color(darkgrey),
			dots=_('$\dots$').scale(0.5),
			l=_('$\left[\right.$').color(blue),
			t = duration ? _(duration).color(blue) : nil,
			ctable(
				r=_('$\left.\right)$').color(blue),
				farR=_('$\left.\right)$').color(darkgrey),
			nil).cmargin(u(-0.1)),
		nil).center,
		path(tcenter(l),t ? tleft(t) : tcenter(r)).thickness(1).color(blue),
		t ? path(tright(t),tcenter(r)).thickness(1).color(blue) : nil,
		p = path(
			tup(dots).post{ |x| x.add(upair(0.0,0.1)) },
			tup(tbl).post{ |x| x.add(upair(0.0,0.2)) },
			t ? tup(t) : tup(l).post{ |x| x.add(upair(0.1,0.0)) },
		nil).arrow.curved.dashed('evenly').color(darkgrey),
	nil)
end

def calendar(txt,img)
	ctable(
		image(img).scale(0.025),
		time(txt),
	nil)
end
def clock(txt,img)
	ctable(
		image(img).scale(0.1),
		time(txt),
	nil)
end
#def dom(n); calendar("$#{n}^{\\textrm{th}}$",'img/months.png'); end
def dom(n); "\\darkred{$#{n}^{\\textrm{th}}$}"; end
def domrd(n); calendar("$#{n}^{\\textrm{rd}}$",'img/months.png'); end
def thursday; calendar('THU','img/months.png'); end
def friday; calendar('FRI','img/months.png'); end
#def friday; '\darkred{\texttt{FRI}}'; end
def monday; calendar('MON','img/months.png'); end
def tuesday; calendar('TUE','img/months.png'); end
def jan; calendar('JAN','img/months.png'); end
def apr; calendar('APR','img/months.png'); end
def nov; calendar('NOV','img/months.png'); end
def sec; clock('SEC','img/times.png'); end
def hour; clock('HOUR','img/times.png'); end
def century; calendar('CENTURY','img/months.png'); end
def everyweek; calendar('EveryWeek','img/months.png'); end
	
def darrow; image('img/darrow.jpg').scale(0.05); end
def uarrow; image('img/uarrow.jpg').scale(0.05); end
def larrow; image('img/larrow.jpg').scale(0.05); end
def rarrow; image('img/rarrow.jpg').scale(0.05); end

def describe(*rows)
	table(*rows.map{ |name,desc|
		name ? [_(name).color(darkblue), desc] : nil
	}).cmargin(u(1.0)).rmargin(u(0.2)).rjustify('c')
end
