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

#--Java Bridge
ENV['JAVA_HOME'] = ENV['JDK_HOME']
Rjb::load(classpath = "#{ENV["JAVANLP_HOME"]}/projects/core/classes", ['-Xmx3000m'])
Runtime = Rjb::import('java.lang.Runtime')
MaxentTagger = Rjb::import('edu.stanford.nlp.tagger.maxent.MaxentTagger')
Word = Rjb::import('edu.stanford.nlp.ling.Word')
ArrayList = Rjb::import('java.util.ArrayList')
TAGGER = MaxentTagger.new('/home/gabor/lib/data/bidirectional-distsim-wsj-0-18.tagger')

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
	(tid, sid, scope_begin, scope_end, type, value, original_value, gloss)
  VALUES(?, ?, ?, ?, ?, ?, ?, ?)")
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
		raise "Bad pub_time: #{self}"\
			if pub_time.length != 2 or pub_time[0] != :INSTANT
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
	attr_accessor :sid, :fid, :words, :pos
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
		File.open("tmp", 'w') {|f| f.write(text) }
		@original = @words
		@words = `tokenize tmp`.split(/\s+/)
		#--Mapping
#		raise "bad tokenization\n#{@words.join('')}\n#{@original.join('')}\n"\
#			if @words.join('').length != @original.join('').length TODO mapping is bad
		#(compute offsets)
		origCharOffset = []
		counter = 0
		@original.each_with_index do |w,i|
			origCharOffset[i] = counter
			counter += w.length
		end
		tokCharOffset = []
		counter = 0
		@words.each_with_index do |w,i|
			tokCharOffset[i] = counter
			counter += w.length
		end
		#(reverse mapping)
		tokOffset2origIndex = (0...@words.join('').length).map do |offset|
			cand = 1
			while cand < origCharOffset.length and origCharOffset[cand] < offset do
				cand += 1
			end
			cand - 1
		end
		#(create map)
		@indexMap = tokCharOffset.map do |offset| tokOffset2origIndex[offset]; end
		#--Tag
		@pos = []
	  #(input) 
	  sent = ArrayList.new
		@words.each do |w|
			sent.add(Word.new(w))
		end
	  #(tag) 
	  tagged = TAGGER.tagSentence(sent)
	  iter = tagged.iterator 
	  while(iter.hasNext) do
	    @pos << iter.next.tag 
	  end 	
		#(cleanup)
		raise "Incorrect tags #{@pos.length} #{@words.length}"\
			if @pos.length != @words.length
	end
	
	def to_db
		process
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
		@indexMap.each_with_index do |origIndex,newIndex|
			ensureStatement(TAG_STMT,
				newIndex+1,
				sid,
				SOURCE_ID,
				'orig',
				origIndex)
		end
	end
end

class Timex
	@@tid = 1
	attr_accessor :tid, :sid, :scope_begin, :scope_end, :type, :value, :original_value, :gloss
	def initialize(sid,start,type,origValue)
		@tid = @@tid
		@sid = sid
		@scope_begin = start.to_i+1
		@type = type
		@original_value = origValue
		@value = parse(origValue,nil)
		@@tid += 1
	end
	def setLength(len)
		raise "zero length" if len == 0
		@scope_end = @scope_begin+len.to_i
	end
	def to_db(gloss)
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
			timexes[doc] = {} if not timexes[doc]
			timexes[doc][sent] = {} if not timexes[doc][sent]
			timexes[doc][sent][tid] =\
				Timex.new(sents[doc][sent].sid,start-1,type,value)
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
	last = nil
	count = 0
	file.each_line do |line|
		doc,sent,start,timex3,tid,junk = line.split(/\s+/)
		sent = sent.to_i
		if [doc,sent,tid] != last and last != nil then
			lastDoc,lastSent,lastTid = last
			timexes[lastDoc][lastSent][lastTid].setLength(count)
			last = [doc,sent,tid]
			count = 0
		elsif last == nil then
			last = [doc,sent,tid]
		end
		count += 1
	end
	#(add last)
	lastDoc,lastSent,lastTid = last
	timexes[lastDoc][lastSent][lastTid].setLength(count)
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
				timex.scope_begin-1,
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



