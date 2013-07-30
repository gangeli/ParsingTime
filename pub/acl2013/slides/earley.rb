#!/usr/bin/ruby
require 'set'
require 'thread'

################################################################################
#Earley Parser
#07/16/2011 Gabor Angeli
#A simple Earley parser implementation; no debugging output yet, and only handles
#completely unambiguous grammars.
#
#Usage:
##(build a grammar)
#grammar = Grammar.new
#grammar.startRule([root], [list_of_children], [fxn_of_children])
#grammar.rule([parent], [list_of_children], [fxn_of_children])
#...
##(parse a string)
#parser = Parser.new([grammar], [tokenizer]?) #tokenizer: f(string) => tokens
#parse  = parser.parse([string]) # also, parser.accepts([string])
#
#Known Bugs:
#	*No debug output
#	*No rule priorities
################################################################################


module Earley

class Grammar
	attr_accessor :rules, :startLeft, :start
	def startRule(left, right, fn)
		@startLeft = left
		@start = [right, fn]
		rule(left,right,fn)
		self
	end
	def rule(left, right, fn)
		@rules = {} if not @rules
		@rules[left] = [] if not @rules[left]
		@rules[left] << [right, fn]
		self
	end
end

class Parser
	class ChartElem
		attr_accessor :parent, :children, :fn, :origin, :dot
		def initialize(parent,children,fn,origin,dot)
			@parent = parent
			@children =children
			@fn = fn
			@origin = origin
			@dot = dot
			@elems = []
		end
		def evaluate
			@fn.call(*@elems.map do |elem|
					if elem.is_a? ChartElem then
						elem.evaluate
					else
						elem
					end
				end)
		end
		def setCompletion(lst); @elems = lst; end
		def onPrix; @children[@dot]; end
		def shiftRight(child); 
			rtn = ChartElem.new(@parent,@children,@fn,@origin,@dot+1); 
			rtn.setCompletion(@elems + [child])
			rtn
		end
		def ==(other)
			parent == other.parent and children == other.children and\
				dot == other.dot and origin == other.origin
		end
		def eql?(other); self == other; end
		def hash
			parent.hash ^ children.hash ^ dot.hash ^ origin.hash
		end
		def complete?; @dot == @children.length; end
		def to_s
			"[#{@origin}] #{@parent.inspect} -> " +
				"#{@children.map{ |x| x.inspect }.insert(@dot,"*").join(" ")}"
		end
	end

	def initialize(*args)
		raise "Bad number of arguments to EarlyParser.new" if args.length == 0
		@grammar = args[0]
		@tokenizer = args.length >= 2 ? args[1] : lambda{ |str| str.split(//) }
		raise "Bad number of arguments to EarlyParser.new" if args.length > 2
	end

	def accepts(input)
		tokens = @tokenizer.call(input)
		def debug(iter,i,elem,str="")
#			left = "s(#{iter})(#{i}) #{elem}"
#			buffer = (0..(40-left.length)).to_a.map{ |x| '' }.join(' ')
#			puts "#{left}#{buffer}# #{str}"
		end
		#--Initialize
		chart = (0..tokens.length).map do |x| []; end
		chart[0][0] =  ChartElem.new(
			parent=@grammar.startLeft, children=@grammar.start[0],
			fn=@grammar.start[1], origin=0, dot=0)
		queue = Queue.new()
		queue << [0,0]
		inQueue = Set.new([])
		incomplete = (0..tokens.length).map do |x| {}; end
		debug(0,1,chart[0][0],"start state")
		#--Parse
		while not queue.empty? do
			#(overhead)
			i,k = queue.pop
			term = chart[i][k]
			if not term.complete?
				incomplete[i][term.onPrix] = [] if not incomplete[i][term.onPrix]
				incomplete[i][term.onPrix] << [i,k]
			end
			#(case: completion)
			if term.complete? then
				incomplete[term.origin][term.parent].each do |a,b|
					toComplete = chart[a][b]
					cand = toComplete.shiftRight(term)
						queue << [i,chart[i].length]
						chart[i] << cand
						inQueue << cand
						debug(i,chart[i].length,cand,"complete from s(#{a})(#{b+1})")
				end if incomplete[term.origin][term.parent]
			#(case: prediction)
			elsif term.onPrix.is_a? Symbol  then
				@grammar.rules[term.onPrix].each do |rule,fn|
					cand = ChartElem.new(parent=term.onPrix,children=rule,
						fn=fn,origin=i,dot=0)
					if not inQueue.member? cand then
						queue << [i,chart[i].length]
						chart[i] << cand
						inQueue << cand
						debug(i,chart[i].length,cand,"predict from s(#{i})(#{k+1})")
					end
				end
			#(case: scanning)
			elsif term.onPrix.is_a? Regexp then
				re = term.onPrix
				if i < tokens.length and tokens[i].match re \
						and	(tokens[i].match re).length == tokens[i].length then
					cand = term.shiftRight(tokens[i])
					queue << [i+1,chart[i+1].length]
					chart[i+1] << cand
					inQueue << cand
					debug(i+1,chart[i+1].length,cand,"scan from s(#{i})(#{k+1})")
				end
			#(case: unknown)
			else
				raise "Illegal State"
			end
		end
		#--Get Parse
		#(find parse)
		parse = nil
		chart[tokens.length].each do |cand|
			if cand.parent == @grammar.startLeft\
					and cand.children == @grammar.start[0]\
					and cand.origin == 0 then
				raise "Ambiguous parse: #{parse.evaluate.inspect} and "+
					"#{cand.evaluate.inspect}" if parse
				parse = cand
			end
		end
		raise "Could not parse: #{input}" if not parse
		#(evaluate parse)
		parse.evaluate
	end

	def parse(input); accepts(input); end
end

TOY = Grammar.new
TOY.startRule(:P, [:S],      lambda{ |s| s } )
TOY.rule(:S, [:S, /\+/, :M], lambda{ |s,p,m| s+m } )
TOY.rule(:S, [:M],           lambda{ |m| m} )
TOY.rule(:M, [:M, /\*/, :T], lambda{ |m,x,t| m * t } )
TOY.rule(:M, [:T],           lambda{ |t| t } )
TOY.rule(:T, [/[0-9]+/],     lambda{ |n| n.to_i } )

TREE = Grammar.new
TREE.startRule(:ROOT, [:EXPR], lambda{ |r| r })
TREE.rule(:EXPR, [:WORD], lambda{ |w| w })
TREE.rule(:EXPR, [:COMMENT], lambda{ |c| nil })
TREE.rule(:EXPR, [:EXPR,:WHITE,:COMMENT], lambda{ |e,s,c| e })
TREE.rule(:EXPR, [:EXPR,:COMMENT], lambda{ |e,c| e })
TREE.rule(:EXPR, [:LPAREN, :EXPR_LST, :RPAREN], lambda{ |l,e,r| e.compact })
TREE.rule(:EXPR_LST, [:EXPR], lambda{ |e| [e] })
TREE.rule(:EXPR_LST, [:EXPR_LST,:WHITE,:EXPR], lambda{ |e,s,l| e + [l] })
TREE.rule(:EXPR_LST, [:EXPR_LST,:COMMENT], lambda{ |e,c| e })
TREE.rule(:WORD, [:UNQUOTED], lambda{ |x| x})
TREE.rule(:WORD, [/"/, :QUOTED, /"/], lambda{ |a,x,b| x})
TREE.rule(:UNQUOTED, [:CHAR], lambda{ |x| x})
TREE.rule(:UNQUOTED, [:UNQUOTED,:CHAR], lambda{ |w,c| w+c })
TREE.rule(:QUOTED, [:QCHAR], lambda{ |x| x})
TREE.rule(:QUOTED, [:QUOTED,:QCHAR], lambda{ |w,c| w+c })
TREE.rule(:CHAR, [/[^\\\(\)\s#"]+/], lambda{ |x| x})
TREE.rule(:CHAR, [/\\/, /./], lambda{ |e,c| c})
TREE.rule(:QCHAR, [:CHAR], lambda{ |x| x})
TREE.rule(:QCHAR, [:WHITE], lambda{ |x| x})
TREE.rule(:QCHAR, [/\(|\)/], lambda{ |x| x})
TREE.rule(:WHITE, [:WHITE_CHR], lambda{ |x| x })
TREE.rule(:WHITE, [:WHITE, :WHITE_CHR], lambda{ |x,y| x + y })
TREE.rule(:WHITE_CHR, [/\s+/], lambda{ |x| x })
TREE.rule(:COMMENT, [/#/, :COMMENT_BODY], lambda{ |h,b| b })
TREE.rule(:COMMENT_BODY, [:COMMENT_BODY, :COMMENT_CHR], lambda{ |b,c| b+c })
TREE.rule(:COMMENT_BODY, [:COMMENT_CHR], lambda{ |c| c })
TREE.rule(:COMMENT_CHR, [/[^\n]/], lambda{ |x| x })
TREE.rule(:LPAREN, [:WHITE, /\(/, :WHITE], lambda{ |a,p,b| p })
TREE.rule(:LPAREN, [/\(/, :WHITE], lambda{ |p,b| p })
TREE.rule(:LPAREN, [:WHITE, /\(/], lambda{ |a,p| p })
TREE.rule(:LPAREN, [/\(/], lambda{ |p| p })
TREE.rule(:RPAREN, [:WHITE, /\)/, :WHITE], lambda{ |a,p,b| p })
TREE.rule(:RPAREN, [/\)/, :WHITE], lambda{ |p,b| p })
TREE.rule(:RPAREN, [:WHITE, /\)/], lambda{ |a,p| p })
TREE.rule(:RPAREN, [/\)/], lambda{ |p| p })

end
