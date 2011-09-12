#!/usr/bin/ruby

require 'rfig/FigureSet'
require "#{ENV['HOME']}/lib/ruby/earley.rb"

class Parse
	@@parser = Earley::Parser.new(Earley::TREE)
	private
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


