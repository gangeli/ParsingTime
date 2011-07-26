#!/usr/bin/ruby

require 'date'
require 'time'
require 'dbi'
require 'xml'
require 'rubygems'
require 'rjb'

require "#{File.dirname(__FILE__)}/../timeutil.rb"

#--Arguments
DIR=ARGV[0]
SCHEMA_DATA=ARGV[1]
if not SCHEMA_DATA or not DIR or ARGV.length > 2 then
  puts "Usage: #{$0} [directory] [dataSchema]"
	exit 1;
end
#--Connection
CONN="DBI:Pg:#{SCHEMA_DATA}:localhost"
USER='research'
PASSWD='what?why42?'
db = DBI.connect(CONN,USER,PASSWD)
#--DB Vars
DOCUMENT="timebank_doc"
SENTENCE="timebank_sent"
TAG="timebank_tag"
TIMEX="timebank_timex"
TLINK="timebank_tlink"
SOURCE="source"
SOURCE_ID="timebank_doc".hash

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
puts "create database {"
puts "  drop tables"
query(db,'
	DROP TABLE timebank_doc CASCADE; 
	DROP TABLE timebank_sent CASCADE; 
	DROP TABLE timebank_tag CASCADE; 
	DROP TABLE timebank_timex CASCADE; 
	DROP TABLE timebank_tlink CASCADE;')
puts "  remove source"
query(db,"DELETE FROM source WHERE did='#{SOURCE_ID}'")
#--Document
query(db,"CREATE TABLE #{DOCUMENT} (
		fid SERIAL PRIMARY KEY,
		filename VARCHAR(127),
		pub_time VARCHAR(31),
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
		temporal_function BOOLEAN,
		mod VARCHAR(15),
		gloss VARCHAR(63)
		);")
#--TLink
query(db,"CREATE TABLE #{TLINK} (
		lid SERIAL PRIMARY KEY,
		fid INTEGER,
		source INTEGER,
		target INTEGER,
		type VARCHAR(15)
		);")
puts "  create statements"
DOC_STMT = db.prepare("INSERT INTO #{DOCUMENT}
	(fid,filename,pub_time,notes)
  VALUES(?, ?, ?, ?)")
SENT_STMT = db.prepare("INSERT INTO #{SENTENCE} 
	(sid,fid,length,gloss)
  VALUES(?, ?, ?, ?)")
TAG_STMT = db.prepare("INSERT INTO #{TAG} 
	(wid, sid, did, key, value)
  VALUES(?, ?, ?, ?, ?)")
TIMEX_STMT = db.prepare("INSERT INTO #{TIMEX} 
	(tid, sid, scope_begin, scope_end, type, value, original_value, 
		temporal_function, mod, gloss)
  VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
TLINK_STMT = db.prepare("INSERT INTO #{TLINK} 
	(lid, fid, source, target, type)
  VALUES(?, ?, ?, ?, ?)")
puts "}"



#-------------------------------------------------------------------------------
# CLASSES
#-------------------------------------------------------------------------------
ENV['JAVA_HOME'] = ENV['JDK_HOME']
Rjb::load(classpath = "#{ENV["JAVANLP_HOME"]}/projects/core/classes", ['-Xmx500m'])
Runtime = Rjb::import('java.lang.Runtime')
MaxentTagger = Rjb::import('edu.stanford.nlp.tagger.maxent.MaxentTagger')
Word = Rjb::import('edu.stanford.nlp.ling.Word')
ArrayList = Rjb::import('java.util.ArrayList')
TAGGER = MaxentTagger.new('/home/gabor/lib/data/bidirectional-distsim-wsj-0-18.tagger')

class Doc
	@@fid = 1
	def self.fid; @@fid; end
	attr_accessor :name, :timexes
	def initialize(name)
		@name = name
		@timexes = []
		@links = []
		@sentences = []
	end
	def pubTime(timex); @pubTime = timex; end
	def time(timex)
		@timexes << timex
	end
	def link(link)
		@links << link if link.time_time?
	end
	def sentence(sent)
		@sentences << sent
	end
	def lastTimex; @timexes[-1]; end
	def lastLink; @links[-1]; end
	def to_s
		@name[0,10]
	end
	def to_db
		#(save self)
		puts "    self"
		raise "No pub_time: #{@name}" if not @pubTime
		raise "Bad pub_time: #{@name}"\
			if @pubTime.value.length != 2 or @pubTime.value[0] != :INSTANT
		ensureStatement(DOC_STMT,
			@@fid,
			name,
			@pubTime.value[1],
			'')
		#(save sentences)
		puts "    sentences (#{@sentences.length})"
		@sentences.each{ |s|
			s.to_db(self)
		}
		#(save links)
		puts "    links (#{@links.length})"
		@links.each{ |l|
			l.to_db(self)
		}
		#(finish)
		puts "    done"
		@@fid += 1
	end
end

class Sent
	@@sid = 1
	def self.sid; @@sid; end
	attr_accessor :doc, :text, :tokens
	def initialize(doc)
		@doc = doc
	end
	def setText(text)
		#--Tokenize
		@text = text
		text = text.strip.gsub(/\//,' / ').gsub(/\s+/,' ').gsub(/\-/,' - ')
		File.open("tmp", 'w') {|f| f.write(text) }
		@tokens = `tokenize tmp`.split(/\s+/)
		#--Tag
		@tags = []
	  #(input) 
	  sent = ArrayList.new
		@tokens.each do |w|
			sent.add(Word.new(w))
		end
	  #(tag) 
	  tagged = TAGGER.tagSentence(sent)
	  iter = tagged.iterator 
	  while(iter.hasNext) do
	    @tags << iter.next.tag 
	  end 	
		#(cleanup)
		raise "Incorrect tags #{@tags.length} #{@tokens.length}"\
			if @tags.length != @tokens.length
	end
	def to_s
		"#{@doc}.sent"
	end
	def to_db(doc)
		#(save self)
		ensureStatement(SENT_STMT,
			@@sid,
			Doc.fid,
			@tokens.length,
			@text)
		#(save tokens)
		wid = 1
		@tokens.each do |token|
			ensureStatement(TAG_STMT,
				wid,
				@@sid,
				SOURCE_ID,
				'form',
				token.strip)
			wid += 1
		end
		#(save tags)
		wid = 1
		@tags.each do |tag|
			ensureStatement(TAG_STMT,
				wid,
				@@sid,
				SOURCE_ID,
				'pos',
				tag.strip)
			wid += 1
		end
		#(save timexes)
		doc.timexes.each do |timex|
			if timex.sent == self then
				timex.to_db(doc,self)
			end
		end
		#(finish)
		@@sid += 1
	end
end

class Timex
	@@tid = 1
	def self.tid; @@tid; end
	attr_accessor :tid, :type, :value, :sent, :dbId
	def initialize(reader,sent)
		for i in 0...reader.attribute_count do
			reader.move_to_attribute(i)
			@tid = reader.value if reader.name == "tid"
			@type = reader.value if reader.name == "type"
			@original_value = reader.value if reader.name == "value"
			@value = parse(reader.value,nil) if reader.name == "value"
			@temporalFn = reader.value if reader.name == "temporalFunction"
			if reader.name == "functionInDocument"\
					and (reader.value == "CREATION_TIME" or\
							reader.value == "PUBLICATION_TIME") then
				sent.doc.pubTime(self)
			end
			@mod = reader.value if reader.name == "mod"
		end
		@sent = sent
	end
	def text
		@text ? @text.chomp : nil
	end
	def addText(target)
		@text = "" if not @text
		@text = @text + " " + target
	end
	def to_s
		"#{tid}: #{value[0]} (#{type}) -- #{text} <<#{sent}>>"
	end
	def to_db(doc,sent)
		#(inefficient index match)
		text = @text.strip.gsub(/\//,' / ').gsub(/\s+/,' ').gsub(/\-/,' - ')
		File.open("tmp", 'w') {|f| f.write(text) }
		tokens = `tokenize tmp`.split(/\s+/)
		startI = nil
		for i in 0..sent.tokens.length do
			startI = i if sent.tokens[i,tokens.length] == tokens
		end
		raise "Could not find substring!" if not startI
		#(clean value)
		value = "[#{@value.join(",")}]"
		#(save self)
		ensureStatement(TIMEX_STMT,
			@@tid,
			Sent.sid,
			startI,
			startI + tokens.length,
			@type,
			value,
			@original_value,
			@temporalFn,
			@mod ? @mod : "NONE",
			text.chomp)
		#(finish)
		@dbId = @@tid
		@@tid += 1
	end
end

class TLink
	@@lid = 1
	def self.lid; @@lid; end
	attr_accessor :source, :relatedTo, :type
	def initialize(reader)
		for i in 0...reader.attribute_count do
			reader.move_to_attribute(i)
			@source = reader.value if reader.name == "timeID"
			@relatedTo = reader.value if reader.name == "relatedToTime"
			@type = reader.value if reader.name == "relType"
		end
	end
	def time_time?; @source and @relatedTo; end
	def to_s
		"#{@source} -[#{@type}]-> #{@relatedTo}"
	end
	def to_db(doc)
		#(get endpoints)
		sourceTime = nil
		targetTime = nil
		doc.timexes.each{ |t|
			sourceTime = t if t.tid == @source
			targetTime = t if t.tid == @relatedTo
		}
		raise "Dangling link #{to_s}" if not sourceTime or not targetTime
		#(save self)
		ensureStatement(TLINK_STMT,
			@@lid,
			Doc.fid,
			sourceTime.dbId,
			targetTime.dbId,
			@type)
		#(finish)
		@@lid += 1
	end
end

#-------------------------------------------------------------------------------
# READ XML
#-------------------------------------------------------------------------------
docs = []
puts "Reading XML {"
#(read)
for file in `find #{DIR} -name "*.tml.xml" | sort` do
	Runtime.getRuntime.gc
	file = file.chomp
	puts "  #{file} {"
	#(vars)
	currDoc = Doc.new(file)
	currSent = nil
	reader = XML::Reader.file(file)
	stack = []
	path = nil
	sent = ""
	#(read loop)
	while reader.read do
	case reader.node_type
		when XML::Reader::TYPE_ELEMENT
			#(case: opening tag)
			if not reader.empty_element? then
				stack << reader.name
				path = nil
			else
			end
			if reader.name == "TIMEX3" then
				if currSent then
					currDoc.time( Timex.new(reader,currSent) ) if currSent
				else
					currDoc.time( Timex.new(reader,Sent.new(currDoc)) ) #use null sentence
				end
			elsif reader.name == "TLINK" then
				link = TLink.new(reader)
				if link.time_time? then
					currDoc.link( link )
					puts "    #{link}"
				end
			elsif reader.name == "s" then
				# note: first sentence is dropped here
				currSent = Sent.new(currDoc)
				sent = ""
			end
		when XML::Reader::TYPE_TEXT, XML::Reader::TYPE_CDATA
			#(case: element)
			path = stack.join('/') if not path
			currDoc.lastTimex.addText(reader.value) if path.include? "TIMEX3"
			sent += " " + reader.value
		when XML::Reader::TYPE_END_ELEMENT
			#(case: closing tag)
			stack.pop
			path = nil
			if reader.name == "TIMEX3" then
				puts "    #{currDoc.lastTimex}"
				raise "No text for timex #{currDoc.lastTimex}"\
					if not currDoc.lastTimex.text or currDoc.lastTimex.text.chomp == ""
			elsif reader.name == "s" then
				currSent.setText(sent.chomp)
				currDoc.sentence(currSent)
				currSent = nil
			end
		end
	end
	puts "  }"
	docs << currDoc
end
puts "}"

#-------------------------------------------------------------------------------
# POPULATE DATABASE
#-------------------------------------------------------------------------------
puts "populating database {"
db['AutoCommit'] = false
puts "  ensuring source"
query(db,"INSERT INTO #{SOURCE} (did,name,notes) VALUES ('#{SOURCE_ID}','timebank','Timebank basic timex annotations')")
docs.each do |doc|
	puts "  writing #{doc.name} {"
	doc.to_db
	puts "  }"
end
db.commit
db['AutoCommit'] = true
puts "}"

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
index(db,TIMEX,"temporal_function")
index(db,TLINK,"type")
fk(db,SENTENCE,DOCUMENT,"fid")
fk(db,TAG,SENTENCE,"sid")
fk(db,TAG,SOURCE,"did")
fk(db,TIMEX,SENTENCE,"sid")
fk(db,TLINK,DOCUMENT,"fid")
fk(db,TLINK,TIMEX,"source","tid")
fk(db,TLINK,TIMEX,"target","tid")

puts "}"

