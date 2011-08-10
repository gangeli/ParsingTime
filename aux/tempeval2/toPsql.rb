#!/usr/bin/ruby

require 'date'
require 'time'
require 'dbi'

require 'rubygems'
require 'rjb'

require "#{File.dirname(__FILE__)}/../timeutil.rb"

#-------------------------------------------------------------------------------
# PARAMETERS
#-------------------------------------------------------------------------------
#--Arguments
LANG=ARGV[0]
SCHEMA_DATA=ARGV[1]
if not SCHEMA_DATA or not LANG or ARGV.length > 2 then
  puts "Usage: #{$0} [language] [dataSchema]"
	exit 1;
end
#--Connection
CONN="DBI:Pg:#{SCHEMA_DATA}:localhost"
USER='research'
PASSWD='what?why42?'
db = DBI.connect(CONN,USER,PASSWD)
#--DB Vars
DOCUMENT="tempeval_#{LANG}_doc"
SENTENCE="tempeval_#{LANG}_sent"
TAG="tempeval_#{LANG}_tag"
TIMEX="tempeval_#{LANG}_timex"
SOURCE="source"
SOURCE_ID="tempeval_#{LANG}".hash

#-------------------------------------------------------------------------------
# CREATE DATABASE
#-------------------------------------------------------------------------------
def query(db,query)
	begin
		puts "  psql> #{query}"
		db.prepare(query).execute()
	rescue DBI::ProgrammingError=>e
		puts e if not  e.to_s.include?("already exists")
	end
end
def ensureStatement(stmt,*args)
	begin
		stmt.execute(*args)
	rescue DBI::ProgrammingError=>e
		if not e.to_s.include? "duplicate key value" then
			puts "FAILED QUERY","------------"
			puts e
			puts stmt
			puts "#{args.join(" ")}"
			puts "--------------"
			puts e
			exit 1
		end
	end
end
#--Clear Last
puts "create database {"
puts "  drop tables"
query(db,"DROP TABLE #{DOCUMENT} CASCADE")
query(db,"DROP TABLE #{SENTENCE} CASCADE")
query(db,"DROP TABLE #{TAG} CASCADE")
query(db,"DROP TABLE #{TIMEX} CASCADE")
puts "  remove source"
query(db,"DELETE FROM source WHERE did='#{SOURCE_ID}'")
#--Document
query(db,"CREATE TABLE #{DOCUMENT} (
		fid SERIAL PRIMARY KEY,
		filename VARCHAR(127),
		pub_time VARCHAR(31),
		test BOOLEAN,
		notes VARCHAR(1023)
		);")
#--Sentence
query(db,"CREATE TABLE #{SENTENCE} (
		sid SERIAL PRIMARY KEY,
		fid INTEGER,
		length INTEGER,
		gloss VARCHAR(2047)
		);")
#--Tag
query(db,"CREATE TABLE #{TAG} (
		wid INTEGER,
		sid INTEGER,
		did INTEGER,
		key VARCHAR(31),
		value VARCHAR(127)
		);")
#--Timex
query(db,"CREATE TABLE #{TIMEX} (
		tid SERIAL PRIMARY KEY,
		sid INTEGER,
		scope_begin INTEGER,
		scope_end INTEGER,
		type VARCHAR(31),
		value VARCHAR(127),
		original_value VARCHAR(127),
		handle VARCHAR(8),
		gold_span VARCHAR(32),
		gloss VARCHAR(63)
		);")
puts "  create statements"
DOC_STMT = db.prepare("INSERT INTO #{DOCUMENT}
	(fid,filename,pub_time,test,notes)
  VALUES(?, ?, ?, ?, ?)")
SENT_STMT = db.prepare("INSERT INTO #{SENTENCE} 
	(sid,fid,length,gloss)
  VALUES(?, ?, ?, ?)")
TAG_STMT = db.prepare("INSERT INTO #{TAG} 
	(wid, sid, did, key, value)
  VALUES(?, ?, ?, ?, ?)")
TIMEX_STMT = db.prepare("INSERT INTO #{TIMEX} 
	(tid, sid, scope_begin, scope_end, type, value, 
		original_value, handle, gold_span, gloss)
  VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
puts "}"

#-------------------------------------------------------------------------------
# CLASSES
#-------------------------------------------------------------------------------
class Doc
	@@fid = 1
	def self.fid; @@fid; end
	attr_accessor :fid, :filename, :pub_time, :test, :notes
	def initialize(filename)
		@filename = filename
		@fid = @@fid
		@@fid += 1
	end

	def to_s
		@filename
	end
	def to_db
		#(save self)
		raise "No pub_time: #{self}" if not pub_time
		ensureStatement(DOC_STMT,
			fid,
			filename,
			pub_time[1],
			test,
			'')
	end
end

class Sent
	@@sid = 1
	def self.fid; @@fid; end
	attr_accessor :sid, :fid, :words, :pos, :original, :orig2retok
	def initialize(fid)
		@fid = fid
		@sid = @@sid
		@@sid += 1
		@words = []
	end

	def process
		#--Tokenize
		text = @words.join(' ')
		text = text.strip.gsub(/\//,' / ').gsub(/\s+/,' ').gsub(/\-/,' - ')
		@original = @words
		@words = tokenizeSentence(text)
		#--Mapping
		#(create map)
		@retok2orig = offsetMap(@original,@words)
		@orig2retok = @original.map do |w| -42; end
		(0 ... @original.length).each do |origI|
			retokI = 0
			while @retok2orig[retokI] != origI and retokI < @words.length do
				retokI += 1
			end
			@orig2retok[origI] = retokI < @words.length ? retokI : -43
		end
		#(clean up)
		@orig2retok.each_with_index do |v,i|
			@orig2retok[i] = @orig2retok[i-1] if v == -43 and i > 0
		end
		@orig2retok[@original.length] = @words.length
		#(print)
		#(check map)
		@retok2orig.each_with_index do |foreign,local|
			raise "retok2orig not sorted"\
				if local>0 and foreign < @retok2orig[local-1]\
				and @retok2orig[local-1] >= 0 and foreign >= 0
		end
		@orig2retok.each_with_index do |v,i|
			raise "invalid value" if v == -42
			raise "orig2retok not sorted" if i > 0 and @orig2retok[i-1] > v
		end
		#--Tag
		@pos = tagSentence(@words)
		#(cleanup)
		raise "Incorrect tags #{@pos.length} #{@words.length}"\
			if @pos.length != @words.length
	end
	
	def to_db
		#(save self)
		ensureStatement(SENT_STMT,
			sid,
			fid,
			words.length,
			words.join(' '))
		#(save tokens)
		words.each_with_index do |word,i|
			ensureStatement(TAG_STMT,
				i+1,
				sid,
				SOURCE_ID,
				'form',
				word.strip)
		end
		#(save POS)
		pos.each_with_index do |pos,i|
			ensureStatement(TAG_STMT,
				i+1,
				sid,
				SOURCE_ID,
				'pos',
				pos.strip)
		end
		#(save index map)
		@retok2orig.each_with_index do |origIndex,newIndex|
			ensureStatement(TAG_STMT,
				newIndex+1,
				sid,
				SOURCE_ID,
				'orig',
				@retok2orig[newIndex])
		end
	end
end

class Timex
	@@tid = 1
	attr_accessor :tid, :sid, :scope_begin, :scope_end, :type, :value, :original_value, :handle, :gloss, :orig_start, :gold_span
	def initialize(sid,start,type,origValue,handle)
		@tid = @@tid
		@sid = sid
		@scope_begin = start.to_i+1
		@type = type
		@original_value = origValue
		@value = parse(origValue,nil)
		@handle = handle
		@@tid += 1
	end
	def setEnd(e)
		raise "non-positive length: #{@scope_begin} to #{e}" if e < @scope_begin
		@scope_end = e+1
	end
	def setGoldSpan(start,stop)
		@gold_span = "[#{start},#{stop}]"
	end
	def to_db(gloss)
		raise "Bad scope: #{scope_begin} to #{scope_end}" if scope_end-scope_begin<1
		#(clean value)
		value = "[#{@value.join(",")}]"
		#(save self)
		ensureStatement(TIMEX_STMT,
			tid,
			sid,
			scope_begin,
			scope_end,
			type,
			value,
			original_value,
			handle,
			gold_span,
			gloss)
	end
end




#-------------------------------------------------------------------------------
# READ FILES
#-------------------------------------------------------------------------------
puts "Reading Files {"
#--Files
puts "  creating files"
train="#{File.dirname(__FILE__)}/training/#{LANG}/data"
test="#{File.dirname(__FILE__)}/test/#{LANG}/key"
base_train = "#{train}/base-segmentation.tab"
base_test  = "#{test}/base-segmentation.tab"
dct_train  = "#{train}/dct.txt"
dct_test   = "#{File.dirname(__FILE__)}/test/#{LANG}/dct-#{LANG[0,2]}.txt"
attr_train = "#{train}/timex-attributes.tab"
attr_test  = "#{test}/timex-attributes.tab"
ext_train  = "#{train}/timex-extents.tab"
ext_test   = "#{test}/timex-extents.tab"

#--Structures
puts "  creating structures"
docs  = {}
sents = {}
timexes = {}

#--Create Documents
puts "  reading {"
#(creation time)
def dct_proc(docs,line,test)
	name,pub_time = line.split(/\s+/)
	docs[name] = Doc.new(name)
	docs[name].pub_time = parse(pub_time,nil)
	docs[name].test = test
end
puts "    dct (train)"
File.new(dct_train).each_line do |line| dct_proc(docs,line,false); end
puts "    dct (test)"
File.new(dct_test).each_line do |line| dct_proc(docs,line,true); end
#(sentences)
def base_proc(sents,docs,line)
	doc,sid,offset,word = line.split(/\s+/)
	sents[doc] = [] if not sents[doc]
	sents[doc][sid.to_i] = Sent.new(docs[doc].fid) if not sents[doc][sid.to_i]
	sents[doc][sid.to_i].words[offset.to_i] = word
end
puts "    sentences (train)"
File.new(base_train).each_line do |line| base_proc(sents,docs,line); end
puts "    sentences (test)"
File.new(base_test).each_line do |line| base_proc(sents,docs,line); end
puts "    process sentences"
sents.each do |doc,sentLst| sentLst.each do |sent| sent.process; end; end
#(timex values)
def attr_proc(timexes,sents,docs,file)
	type = nil
	value = nil
	file.each_line do |line|
		doc,sent,start,timex3,tid,junk,tag,val = line.split(/\s+/)
		sent = sent.to_i
		start = start.to_i
		if tag == "type" then
			type = val
		else
			value = val
		end
		if type and value then
			newStart = sents[doc][sent].orig2retok[start]
			timexes[doc] = {} if not timexes[doc]
			timexes[doc][sent] = {} if not timexes[doc][sent]
			timexes[doc][sent][tid] =\
				Timex.new(sents[doc][sent].sid,newStart,type,value,tid)
			timex = timexes[doc][sent][tid]
			timex.orig_start = start
			raise "start longer than length"\
				if timex.orig_start >= sents[doc][sent].original.length
			type = nil; value = nil
		end
	end
end
puts "    timex attributes (train)"
attr_proc(timexes,sents,docs,File.new(attr_train))
puts "    timex attributes (test)"
attr_proc(timexes,sents,docs,File.new(attr_test))

#(timex spans)
def ext_proc(timexes,sents,docs,file)
	def updateFromCount(timexes,sents,lastDoc,lastSent,lastTid,count,start)
		timex = timexes[lastDoc][lastSent][lastTid]
		beginVal = sents[lastDoc][lastSent].orig2retok[timex.orig_start]
		endConservative = 
			sents[lastDoc][lastSent].orig2retok[timex.orig_start+count]
		endGreedy = sents[lastDoc][lastSent].orig2retok[timex.orig_start+count+1]-1
		endVal = endConservative > endGreedy ? endConservative : endGreedy
		raise "bad count" if count <= 0
		raise "bad map #{beginVal} to #{endVal}" if endVal <= beginVal
		raise "no end val #{count}" if not endVal
		timex.setEnd( endVal )
		timex.setGoldSpan( start.to_i, start.to_i+count )
	end
	last = nil
	lastStart = nil
	count = 0
	file.each_line do |line|
		doc,sent,start,timex3,tid,junk = line.split(/\s+/)
		sent = sent.to_i
		if [doc,sent,tid] != last and last != nil then
			#(update timex)
			lastDoc,lastSent,lastTid = last
			updateFromCount(timexes,sents,lastDoc,lastSent,lastTid,count,lastStart)
			last = [doc,sent,tid]
			lastStart = start
			count = 0
		elsif last == nil then
			last = [doc,sent,tid]
			lastStart = start
		end
		count += 1
	end
	#(update last)
	lastDoc,lastSent,lastTid,lastStart = last
	updateFromCount(timexes,sents,lastDoc,lastSent,lastTid,count,lastStart)
end
puts "    timex extents (train)"
ext_proc(timexes,sents,docs,File.new(ext_train))
puts "    timex extents (test)"
ext_proc(timexes,sents,docs,File.new(ext_test))

puts "  }"
puts "}"

#-------------------------------------------------------------------------------
# WRITE DATABASE
#-------------------------------------------------------------------------------
#--Source
query(db,"INSERT INTO #{SOURCE} (did,name,notes) VALUES ('#{SOURCE_ID}','tempeval-#{LANG}','Tempeval2 Data for #{LANG}')")
#--Write
#(begin)
db['AutoCommit'] = false
puts "Writing to DB {"
#(documents)
puts "  documents"
docs.each do |filename,doc| doc.to_db; end
#(sentence)
puts "  sentences"
sents.each do |doc,sentLst| sentLst.each do |sent| sent.to_db; end; end
#(timexes)
puts "  timexes"
timexes.each do |doc,sentLst| 
	sentLst.each do |sent,timexes| 
		timexes.each do |timex_tag,timex| 
			words = sents[doc][sent].words
			segment = words.slice(
				timex.scope_begin-1, #compensate for +1 in Timex class
				timex.scope_end-timex.scope_begin)
			gloss = segment.join(' ')
			timex.to_db(gloss)
		end
	end
end
#(finish)
db.commit
db['AutoCommit'] = true
#--Index
puts "creating indices {"
def index(db,table,field,method="BTREE")
	puts "  #{table}.#{field}"
	begin
		db.prepare("DROP INDEX #{table}_#{field}").execute()
	rescue Exception
	end
	begin
		db.prepare("CREATE INDEX #{table}_#{field} ON #{table} USING #{method} (#{field})").execute()
		db.commit
	rescue DBI::ProgrammingError=>e
		puts e if not  e.to_s.include?("already exists")
	end
end
def fk(db,table1,table2,field,field2=nil)
	field2 = field2 ? field2 : field
	index(db,table1,field,"BTREE");
	puts "  #{table1}.#{field} REF #{table2}.#{field}"
	begin
		db.prepare("ALTER TABLE #{table1} ADD CONSTRAINT #{table1}_fk_#{field} FOREIGN KEY (#{field}) REFERENCES #{table2}(#{field2})").execute()
		db.commit
	rescue DBI::ProgrammingError=>e
		puts e if not  e.to_s.include?("already exists")
	end
end

index(db,SENTENCE,"length")
index(db,TAG,"wid")
index(db,TAG,"did")
index(db,TAG,"key")
index(db,TIMEX,"type")
index(db,TIMEX,"scope_begin")
index(db,TIMEX,"scope_end")
fk(db,SENTENCE,DOCUMENT,"fid")
fk(db,TAG,SENTENCE,"sid")
fk(db,TAG,SOURCE,"did")
fk(db,TIMEX,SENTENCE,"sid")
puts "}"



