#!/usr/bin/ruby

# A small macro language that helps organize the running of executions with
# different settings and combinations.
# Options:
# -d (debug); otherwise, run it remotely using wq
# -n (print out the command that would be executed)
# -t: run a thunk
# @<tag>: run commands only with the environment set

# Special variables (set via, for example, let(:tag, 'foobar'))
# tag: used to match the tag that's passed in via options
# easy: if command fails, press on
# appendArgs: arguments to put at very end

require 'runfig'

class ExecRunner
  attr :extraArgs
  def initialize(prog, extraArgs)
    @prog = prog
    setExtraArgs(extraArgs)
  end
  def setExtraArgs(extraArgs)
    @debug = extraArgs.member?("-d") # Just pass %debug to runfig
    @thunk = extraArgs.member?("-t") # Just pass %thunk to runfig
    @pretend = extraArgs.member?("-n") # Print out what would be passed to runfig (less detail than %pretend)
    @specifiedTags = extraArgs.map { |x| x =~ /^@(.+)$/ ? $1 : nil }.compact

    # Remove the options and tags that we just extracted
    @extraArgs = extraArgs.clone.delete_if { |x|
      x =~ /^-[dnt]$/ || x =~ /^@/
    }
  end

  # Specify tag to run a command
  def requireTags(v=true); @requireTags = v; nil end

  class Prog
    attr_accessor :x
    def initialize(x); @x = x; end
  end
  def prog(x); Prog.new(x) end
  class Let
    attr_accessor :var, :values
    def initialize(var, values); @var = var; @values = values end
  end
  def let(var, *values); Let.new(var, values.size == 0 ? nil : values) end
  class Prod
    attr_accessor :choices
    def initialize(choices); @choices = choices end
  end
  def prod(*choices)
    # Make sure each choice is an array.
    Prod.new(ExecRunner.standarizeList(choices).map {|choice| choice.class == Array ? choice : [choice]})
  end
  class Stop
  end
  def stop; Stop.new end

  def opt(key, *values)
    values = ExecRunner.standarizeList(values.flatten).map { |value| value && value.to_s }
    # Quote values: -x -0.5 => -x --- -0.5 ---
    values = ['---']+values+['---'] if values.map { |x| x =~ /^-/ ? x : nil }.compact.size > 0
    ["-#{key}"] + values
  end
  def optAppend(key, *values)
    values = ExecRunner.standarizeList(values.flatten).map { |value| value && value.to_s }
    ["+#{key}"] + values
  end

  def optName(key, name, *values);
    if values == [true] then
      opt = name
    elsif values == [false] then
      opt = "!"+name
    elsif name then
      opt = "#{name}=#{values.join(',')}"
    else
      opt = values.join(',')
    end
    opt(key, values) + optAppend('miscOptions', opt)
  end
  def prodOpt(key, *values)
    prod(*values.map { |value| opt(key, value) })
  end
  def prodOptName(key, name, *values)
    prod(*values.map { |value| optName(key, name, value) })
  end
  # Shorter names: laziness
  # Uppercase means has name and will go in miscOptions
  # P means product
  def o(key, *values); opt(key, *values) end
  def O(key, *values); optName(key, key, *values) end # Use default name
  def po(key, *values); prodOpt(key, *values) end
  def ON(key, name, *values); optName(key, name, *values) end
  def PO(key, *values); prodOptName(key, key, *values) end
  def PON(key, name, *values); prodOptName(key, name, *values) end
  def a(key, *values); optAppend(key, *values) end

  class Env
    attr_accessor :list
    def initialize(isTerminal, list)
      @isTerminal = isTerminal
      @list = ExecRunner.standarizeList(list.flatten)
    end

    def getRuns(runner, list=@list, args=[], bindings={})
      if list.size == 0
        runner.runArgs(args, bindings) if @isTerminal
        return
      end

      x, *rest = list
      case x
      when Array then
        getRuns(runner, x+rest, args, bindings)
      when Let then # Temporarily modify bindings
        oldvalues = bindings[x.var]
        bindings[x.var] = x.values
        getRuns(runner, rest, args, bindings)
        bindings[x.var] = oldvalues
      when Symbol # Substitute bindings with something else
        raise "Variable not bound: '#{x}'" unless bindings[x]
        getRuns(runner, bindings[x]+rest, args, bindings)
      when Prod then # Branch on several choices (each choice is a list)
        x.choices.each { |choice|
          getRuns(runner, choice+rest, args, bindings)
        }
      when Env then # Branch
        x.getRuns(runner, x.list, args, bindings)
        getRuns(runner, rest, args, bindings)
      when Prog then # Set the program
        getRuns(runner, rest, args+[x], bindings) # Just add to arguments
      else # String
        getRuns(runner, rest, args+[x.to_s], bindings) # Just add to arguments
      end
    end

    def to_s; "env(#{@list.size})" end
  end
  def env(*list); Env.new(false, list) end
  def run(*list); Env.new(true, list) end

  # Disable/enable the runs (not used very often)
  def disable; let(:disable, true) end
  def enable; let(:disable) end
  # Disabled versions (shortcuts for disabling)
  def denv(*list); nil end
  def drun(*list); nil end
  def dprod(*list); nil end

  def sel(i, *list); i == nil ? prod(*list) : list.compact[i] end
  def selo(i, name, *args); i == nil ? prodOpt(name, *args) : opt(name, args.compact[i]) end
  def dsel(*list); nil end
  def dselo(*list); nil end
  def l(*list); list end
  def dl(*list); nil end

  def tag(*v); let(:tag, *v) end
  def easy(v=true); let(:easy, v) end
  def ignoreExtraArgs(v=true); let(:ignoreExtraArgs, v) end
  def appendArgs(*v); let(:appendArgs, *v) end

  def memberOfAny(a, b)
    return false unless b
    b.each { |x|
      return true if a.member?(x)
    }
    false
  end

  # Run the command with the arguments
  def runArgs(args, bindings)
    return if bindings[:disable]
    #puts @specifiedTags.inspect
    #puts bindings[:tag].inspect
    # Skip if user specified some tags but none of the current tags
    # match any of the specified tags
    if @requireTags || @specifiedTags.size > 0
      return if (not memberOfAny(@specifiedTags, bindings[:tag]))
    end

    args = args.clone
    if @debug    then args << "%debug"
    elsif @thunk then args << "%thunk"
    end
    args += @extraArgs unless bindings[:ignoreExtraArgs]
    args += bindings[:appendArgs] if bindings[:appendArgs]

    prog = args.reverse.find { |x| x.is_a?(Prog) }
    args.delete_if { |x| x.is_a?(Prog) }
    if prog
      if @pretend
        puts prog.x.inspect + " " + args.join(" ")
      else
        prog.x[:argv] = args
        runfig2(prog.x)
      end
    else
      # If no explicit program given, just concatenate arguments together
      # (assume first argument is the program)
      cmd = args.join(" ")
      if @pretend then
        puts cmd
      else
        exit 1 if (not system cmd) && (not bindings[:easy])
      end
    end
  end

  # Execute the environment
  def execute(e)
    e.getRuns(self)
  end 

  # Helper function: compact the list of arguments
  # Also, remove anything after stop object
  def ExecRunner.standarizeList(list)
    hasStopped = false
    list.map { |x|
      hasStopped = true if x.is_a?(Stop) 
      hasStopped ? nil : x
    }.compact
  end

  # Simple way to process command-line arguments
  # Return [value1, ... valueK]; modifies args
  def extractArgs(args, remove, *names)
    newArgs = []
    values = [nil] * names.size
    i = nil
    args.each { |arg|
      if arg =~ /^-(.+)$/ then
        i = names.index($1)
        #puts $1, names.inspect
        values[i] = [] if i
      else
        values[i] << arg if i
      end
      newArgs << arg unless remove && i
    }
    args.clear
    newArgs.each { |arg| args << arg }
    setExtraArgs(args) # Update in case we removed some arguments
    #args += newArgs # Stupid; doesn't work
    #puts "ARG: #{args.inspect}"
    values
  end
end

############################################################
# How to use:
# env!(
#   run(prog('grape'), o('maxIters', 4)),
#   run(prog(:className => 'grape.Main'), o('maxIters', 4)),
# nil)
$globalExecRunner = ExecRunner.new(nil, ARGV)
def requireTags(v=true); $globalExecRunner.requireTags(v) end
def env!(*x); execute(env(*x)) end
def execute(*x); $globalExecRunner.execute(*x) end
def env(*x); $globalExecRunner.env(*x) end
def run(*x); $globalExecRunner.run(*x) end
def prod(*x); $globalExecRunner.prod(*x) end
def stop(*x); $globalExecRunner.stop(*x) end
def dl(*x); $globalExecRunner.dl(*x) end
def denv(*x); $globalExecRunner.denv(*x) end
def drun(*x); $globalExecRunner.drun(*x) end
def dprod(*x); $globalExecRunner.dprod(*x) end
def sel(*x); $globalExecRunner.sel(*x) end
def selo(*x); $globalExecRunner.selo(*x) end
def dsel(*x); $globalExecRunner.dsel(*x) end
def dselo(*x); $globalExecRunner.dselo(*x) end
def l(*x); $globalExecRunner.l(*x) end
def o(*x); $globalExecRunner.o(*x) end
def po(*x); $globalExecRunner.po(*x) end
def a(*x); $globalExecRunner.a(*x) end
def O(*x); $globalExecRunner.O(*x) end
def ON(*x); $globalExecRunner.ON(*x) end
def tag(*x); $globalExecRunner.tag(*x) end
def prog(*x); $globalExecRunner.prog(*x) end

def note(*x); a('note', *x) end
def misc(*x); a('miscOptions', *x) end

# To use the same execrunner script on many machines,
# we need to dynamically figure out the view to add an execution to
# Assume $CLUSTER is set
# Example usages: view(3)
#                 view('s' => 3, 'r' => 5)
def view(map)
  if map.is_a?(Hash)
    x = map[ENV['CLUSTER']]
    x && o('addToView', x)
  else
    o('addToView', map)
  end
end
def tagview(x); l(view(x), tag(x)) end
