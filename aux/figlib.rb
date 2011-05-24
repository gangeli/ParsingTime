#!/usr/bin/ruby

require 'rfig/FigureSet'

class Parse
	private
	#-----
	# PARSE
	#-----
	def parenparse(input) #TODO not really correct (escaped \( \), quotes, etc)
		def recurse(data,indent=0) # returns [sub_parse, remaining_terms]
			def isEscaped(content)
				not content.match(/^\s*$/) and content[-1..-1] == '\\'
			end
			#--Variables
			lst = []
			content, matched = data[0]
			data = data[1..-1]
			while content and (isEscaped(content) or not matched.match /^\)$/) do
				#--Open Paren
				if matched.match /^\($/ and not isEscaped(content) then
					lst << content  if not content.match /^\s*$/
					term, data = recurse(data,indent+1)
					lst << term.compact if term
				#--Content
				else
					content = "#{content[0...-1]}#{matched}"\
						if isEscaped(content) #escape characters matched
					content = nil if content.match /^\s*$/ #empty nonsense
					lst << content if content
				end
				content, matched = data[0]
				data = data[1..-1]
			end
			#--Global end
			if not content then
				[lst,data]
			#--Close Paren
			elsif content.match /^\s*$/ then
				[lst,data]
			else
				lst << content if content
				[lst, data]
			end
		end
		#--Clean Special Characters
		def clean(lst)
			rtn = []
			lst.each{ |term|
				if term.is_a? Array then
					rtn << clean(term)
				else
					#(get last element)
					last = rtn[-1]
					#(if last element was escaped; compress)
					if last and (last[-1..-1] == "(" or last[-1..-1] == ")") then
						rtn = rtn[0...-1]
						rtn << "#{last}#{term}"
					#(else add)
					else
						rtn << term
					end
				end
			}
			rtn
		end
		def spacify(lst)
			rtn = []
			lst.map{ |term|
				if term.is_a? Array then
					spacify(term)
				else
					term.gsub(/_/,' ')
				end
			}
		end
		recurse( input.scan(/(.*?)(\s+|\)|\()/) )[0][0]
	end
	#-----
	# RFIG UTIL
	#-----
	def head(a)
		_(a)
	end
	def leaf(a)
		_("{\\it #{a}}")
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
	# RENDER
	#-----
	def constituency
		def subTree(lst) #[tree, root, [[edge_begin,edge_end], ...]
			if not lst.is_a? Array then
				leaf = leaf(lst)
				[leaf,leaf,[]] # base case
			else
				#--Argument Parse
				raise "Too few arguments in tree #{lst}" if lst.length == 0
				#(head)
				head = head(lst[0])
				#(children -- recursive case)
				rec = lst[1..-1].map{ |term| subTree(term) }
				children = rec.map{ |tuple| tuple[0] }
				edges = rec.map{ |tuple| [head,tuple[1]] }
				rec.each{ |tuple| tuple[2].each{ |e| edges << e } }
				#--Render Return
				[
					node(*[head,*children]),
					head,
					edges
				]
			end
		end
		info = subTree(@input)
		tree = info[0]
		root = info[1]
		edges = info[2]
		tree
		overlay(tree, *edges.map{ |pair| synedge(*pair) })
	end
	#-----
	# OVERHEAD
	#-----
	def initialize(input)
		if input.is_a? Array
			@input = input
		else
			@input = parenparse(input)
			puts to_s
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


