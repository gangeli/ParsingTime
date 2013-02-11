#!/usr/bin/ruby

require 'rfig/FigureSet'
require "#{ENV['HOME']}/lib/ruby/earley.rb"

################################################################################
# CLASS MODIFICATIONS
################################################################################

class Array
	def color(c)
		self.each_with_index{ |term,i|
			self[i] = recursiveColor(term,c)
		}
		self
	end
end

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
		_(txt).color(nocolor)
	end
end

def soft(cond, txt)
	if(cond) then
		_(txt)
	else
		_(txt).color(grey)
	end
end

def choose(num, *args)
	elems = []
	args.each_with_index{ |term,i|
		if(i != num and term) then
			elems << _(term).color(nocolor)
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
		if(a.is_a? String or a.is_a? Str) then
			_("\\textit{#{a}}")
		else
			a
		end
	end
	def node(rspace,cspace,thickness,*args)
		def vert(rspace,*args)
			rtable(*args).center.rmargin(u(rspace))
		end
		def horiz(cspace,*args)
			ctable(*args).cmargin(u(cspace))
		end
		vert(rspace,*[args[0], horiz(cspace,*args[1..-1])])
	end
	def synedge(a,b)
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
