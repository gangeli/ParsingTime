require 'rubygems'
require 'rjb'

# -- JAVA --
ENV['JAVA_HOME'] = ENV['JDK_HOME']
Rjb::load(classpath = "#{ENV["JAVANLP_HOME"]}/projects/core/classes", ['-Xmx3000m'])
Runtime = Rjb::import('java.lang.Runtime')
MaxentTagger = Rjb::import('edu.stanford.nlp.tagger.maxent.MaxentTagger')
DocumentPreprocessor = Rjb::import('edu.stanford.nlp.process.DocumentPreprocessor')
Word = Rjb::import('edu.stanford.nlp.ling.Word')
HasWord = Rjb::import('edu.stanford.nlp.ling.HasWord')
List = Rjb::import('java.util.List')
ArrayList = Rjb::import('java.util.ArrayList')

TAGGER = MaxentTagger.new('/home/gabor/lib/data/bidirectional-distsim-wsj-0-18.tagger')

def parse(str,ground)
	str = str.chomp
	# -- TIME --
	if str.match /^T[0-9]{4}$/ then
		hr = str[0..1].to_i
		min = str[2..3].to_i
		[:INSTANT, DateTime.civil(ground.year,ground.month,ground.day,hr,min)]
	# -- REAL TIMES --
	#(year)
	elsif str.match /^[0-9]{2,4}$/ then
		str = "19"+str if str.length == 2
		if str.length == 3 then
			decade = str.to_i
			[:RANGE, 
				DateTime.civil(decade*10),
				DateTime.civil((decade+1)*10)
			]
		else
			[:RANGE, 
				DateTime.civil(str.to_i),
				DateTime.civil(y=str.to_i+1)
			]
		end
	#(yearmonth)
	elsif str.match /^[0-9]{6}$/ then
		year = str[0..3].to_i
		month = str[4..5].to_i
		[:RANGE, 
			DateTime.civil(y=year,m=month),
			DateTime.civil((month==12 ? year+1 : year),(month%12)+1)
		]
	#(yearWweek)
	elsif str.match /^[0-9]{4}W[0-9 ]{2}$/ then
		terms = [str[0..3],str[-2..-1]]
		[:RANGE,
			DateTime.commercial(terms[0].to_i,terms[1].to_i,1),
			DateTime.commercial(
				terms[1].to_i == 52 ? terms[0].to_i+1 : terms[0].to_i,
				((terms[1].to_i)%52)+1,1)
		]
	#(year-month)
	elsif str.match /^[0-9]{4}-[0-9 ]{2}$/ then
		terms = str.split(/-/)
		[:RANGE,
			DateTime.civil(terms[0].to_i,terms[1].to_i),
			DateTime.civil(
				terms[1].to_i == 12 ? terms[0].to_i+1 : terms[0].to_i,
				((terms[1].to_i)%12)+1)
		]
	#(year-quarter)
	elsif str.match /^[0-9]{4}-Q[0-9]$/ then
		terms = str.split(/-/)
		yr = terms[0].to_i
		qstart = (terms[1][1,2].to_i-1)*3 + 1
		qend = (((terms[1][1,2].to_i)*3)%12) + 1
		[:RANGE,
			DateTime.civil(yr,qstart),
			DateTime.civil(qend < qstart ? yr+1 : yr, qend)
		]
	elsif str.match /^[0-9]{4}-H[0-9]$/ then
		terms = str.split(/-/)
		yr = terms[0].to_i
		qstart = (terms[1][1,2].to_i-1)*6 + 1
		qend = (((terms[1][1,2].to_i)*6)%12) + 1
		[:RANGE,
			DateTime.civil(yr,qstart),
			DateTime.civil(qend < qstart ? yr+1 : yr, qend)
		]
	#(year-season)
	elsif str.match /^[0-9]{4}-?((SP)|(SU)|(FA)|(WI))$/ then
		terms = [str[0..3],str[-2..-1]]
		yr = terms[0]
		if(terms[1] == "WI") then
			parse("#{yr}-Q1",ground)
		elsif(terms[1] == "SP") then
			parse("#{yr}-Q2",ground)
		elsif(terms[1] == "SU") then
			parse("#{yr}-Q3",ground)
		elsif(terms[1] == "FA") then
			parse("#{yr}-Q4",ground)
		else
			raise "UNKNOWN SEASON #{terms[1]} : #{str}"
		end
	# -- RELATIVE TIMES --
	#(past)
	elsif str == "PAST_REF" then
		[:RANGE, :x, :NOW]
	#(future)
	elsif str == "FUTURE_REF" then
		[:RANGE, :NOW, :x]
	#(present)
	elsif str == "PRESENT_REF" then
		[:INSTANT, :NOW]
	#(unhandled)
	# -- PERIOD --
	elsif str.match /^P([0-9]*(D|W|M|Q|Y|E|C|L|H|S|T))+$/ then
		time = false
		period = [:PERIOD, 0, 0, 0, 0, 0, 0, 0] #tag|year|month|week|day|hr|min|sec
		str.scan(/[0-9]*[DWMQYECLHST]/).each{ |d|
			if d[-1].chr == "L" then
				period[1] = d[0,d.length-1].to_i*1000
			elsif d[-1].chr == "C" then
				period[1] = d[0,d.length-1].to_i*100
			elsif d[-1].chr == "E" then
				period[1] = d[0,d.length-1].to_i*10
			elsif d[-1].chr == "Y" then
				period[1] = d[0,d.length-1].to_i
			elsif d[-1].chr == "Q" then
				period[2] = d[0,d.length-1].to_i*3
			elsif d[-1].chr == "M" then
				period[time ? 6 : 2] = d[0,d.length-1].to_i
			elsif d[-1].chr == "W" then
				period[3] = d[0,d.length-1].to_i
			elsif d[-1].chr == "D" then
				period[4] = d[0,d.length-1].to_i
			elsif d[-1].chr == "H" then
				period[5] = d[0,d.length-1].to_i
			elsif d[-1].chr == "S" then
				period[7] = d[0,d.length-1].to_i
			elsif d[-1].chr == "T" then
				time = true
			else
				raise "UNKNOWN INTERVAL #{d}"
			end
		}
		period
	elsif str.include? "X" then
		[:UNK, str]
	else
		begin
			#(ruby's parse time)
			[:INSTANT, DateTime.parse(str)]
		rescue Exception
			raise "UNKNOWN TIME EXPRESSION #{str}"
		end
	end
end

def parseStr(str)
	array = str[1...-1].split /,/
	array[0] = array[0].to_sym
	array
end

def sameTime(time1, time2, grounding)
	def ground(time,grounding)
		if time[0] == :INSTANT and not time[1].is_a? DateTime then
			if time[1].to_sym == :NOW then
				time[1] = grounding
			end
		elsif time[0] == :RANGE and\
				not time[1].is_a? DateTime and not time[2].is_a? DateTime then
			if time[1].to_sym == :NOW and time[2].to_sym == :x then
				time[1] = grounding
				time[2] = DateTime.civil(10000)
			elsif time[1].to_sym == :x and time[2].to_sym == :NOW then
				time[1] = DateTime.civil(-10000)
				time[2] = grounding
			end
		end
		time
	end
	time1 = ground(time1,grounding)
	time2 = ground(time2,grounding)
	time1 = [:RANGE, time1[1], time1[1]] if time1[0] == :INSTANT
	time2 = [:RANGE, time2[1], time2[1]] if time2[0] == :INSTANT
	if time1[0] == :RANGE and time2[0] == :RANGE then
		#(range: max day total offset match)
		start1 = DateTime.parse(time1[1].to_s)
		start2 = DateTime.parse(time2[1].to_s)
		end1 = DateTime.parse(time1[2].to_s)
		end2 = DateTime.parse(time2[2].to_s)
		startGap = (start2-start1).to_f.abs
		endGap = (end2-end1).to_f.abs
		(startGap+endGap) <= 1.0
	elsif time1[0] == :PERIOD and time2[0] == :PERIOD then
		#(duration: day match)
		year1,month1,week1,day1,hour1,minute1,second1 = time1[1..-1]
		year2,month2,week2,day2,hour2,minute2,second2 = time1[1..-1]
		return (year1==year2 and month1==month2 and week1==week2 and day2==day1)
	else
		#(times and durations are not convertable)
		false
	end
end


def tokenize(text)
	#(process)
	if text.is_a? Array then
		text = text.map do |sent|
			sent.join(' ') if sent.is_a? Array
		end
		text = text.join('
')
	end
	#(write to file)
	tmp = `mktemp`
	File.open(tmp, 'w') {|f| f.write(text) }
	#(tokenize)
	sents = []
	tokenizer = DocumentPreprocessor.new(tmp)
  iter = tokenizer.iterator 
  while(iter.hasNext) do
		lst = iter.next
		iterSent = lst.iterator
		sent = []
		while iterSent.hasNext do
			w = iterSent.next
			sent << w.word
		end
		sents << sent
  end 	
	sents
end

def tokenizeSentence(sent)
	sent = sent.join(' ') if sent.is_a? Array
	tokenize(sent).flatten
end

def tagSentence(words)
	words = words.split(/\s+/) if words.is_a? String
	pos = []
  #(input) 
  sent = ArrayList.new
	words.each do |w|
		sent.add(Word.new(w))
	end
  #(tag) 
  tagged = TAGGER.tagSentence(sent)
  iter = tagged.iterator 
  while(iter.hasNext) do
    pos << iter.next.tag 
  end 	
	pos
end


def offsetMap(orig,retoken,greedy=true)
	#(simple maps)
	orig = orig.map do |word| word == '"' ? "''" : word; end
	orig = orig.map do |word| word == '(' ? "-LXB-" : word; end
	orig = orig.map do |word| word == ')' ? "-RXB-" : word; end
	orig = orig.map do |word| word == '{' ? "-LXB-" : word; end
	orig = orig.map do |word| word == '}' ? "-RXB-" : word; end
	orig = orig.map do |word| word == '[' ? "-LXB-" : word; end
	orig = orig.map do |word| word == ']' ? "-RXB-" : word; end
	retoken = retoken.map do |word| word == '-LRB-' ? "-LXB-" : word; end
	retoken = retoken.map do |word| word == '-RRB-' ? "-RXB-" : word; end
	retoken = retoken.map do |word| word == '-LCB-' ? "-LXB-" : word; end
	retoken = retoken.map do |word| word == '-RCB-' ? "-RXB-" : word; end
	retoken = retoken.map do |word| word == '-LSB-' ? "-LXB-" : word; end
	retoken = retoken.map do |word| word == '-RSB-' ? "-RXB-" : word; end
	#(bloody brits)
	orig = orig.map do |word| word == 'theatre' ? 'theater' : word; end
	#--Util
	#(variables)
	infoOrig = [0,0]
	infoRetoken = [0,0]
	#(iterator)
	def nextChar(array,info)
		w,offset = info
		info[1] += 1
		if info[0] < array.length and info[1] >= array[info[0]].length then
			info[0] += 1
			info[1] = 0
		end
		if w >= array.length then
			nil
		else
			array[w][offset].chr
		end
	end
	#--Setup
	mapping = retoken.map do |i| -42; end
	#--Map
	chOrig = nextChar(orig,infoOrig)
	chRetok = nextChar(retoken,infoRetoken)
	while chOrig and chRetok do
		#(fixes)
		chRetok = 's' if chRetok == 'z' and chOrig == 's'\
			if "#{chRetok}".strip != "#{chOrig}".strip #bloody brits
		#(align)
		if "#{chRetok}".strip != "#{chOrig}".strip then
			sav = chRetok
			mapping[infoRetoken[0]-1] = -1\
				if greedy or mapping[infoRetoken[0]-1] == -42
			chRetok = nextChar(retoken,infoRetoken)
			puts orig.join(' ') if not chOrig == chRetok
			puts retoken.join(' ') if not chOrig == chRetok
			raise "not same #{chOrig} #{chRetok} [ #{sav} ]" if not chOrig == chRetok
		else
			#(save)
			mapping[infoRetoken[0]] = infoOrig[0]\
				if infoOrig[0] < orig.length and\
				(greedy or mapping[infoRetoken[0]] == -42)
		end
		#(iter)
		chOrig = nextChar(orig,infoOrig)
		chRetok = nextChar(retoken,infoRetoken)
	end
	#--Clean Up
	#(end)
	while nextChar(retoken,infoRetoken) do
			mapping[infoRetoken[0]] = orig.length-1
	end
	#(skipped terms)
	mapping.each_with_index do |term,i|
		if term == -42 then
			if i == 0 then
				mapping[i] = 0
			else
				mapping[i] = mapping[i-1]
			end
		end
	end
	#--Return
	mapping
end



#orig = 'It\'s a sentence Inc. New sentence for $50'.split /\s+/
#retok = 'It \'s a sentence Inc. . New sentence for $ 50'.split /\s+/
#offsetMap(orig,retok).each_with_index do |o,r|
#	puts "#{r} #{o}: #{retok[r]} -> #{orig[o]}"
#end




