# Provides a wrapper around a fig-enabled Java/Scala program.
# Features: supports loading of Jar files, setting up
# the execution pool directory.
#
# You no longer need to conform to this strict set of conventions.
# All you need is to pass in a class name.
# package is the name of the project (cross, cortex, etc.)
# PACKAGE is the uppercase version
# Environment variables:
#   JAVA_OPTS, FIG_DIR, <PACKAGE>_DIR
#   Optional: $<PACKAGE>_JAR: location of the 
# Default file locations:
#  - figJarPath: $FIG_DIR/fig.jar
#  - baseDir: $<PACKAGE>_DIR
#  - packageJarPath: <baseDir>/code/java/<package>.jar (OR $<PACKAGE>_JAR)
#  - <baseDir>/bin/usage.txt
#  - <baseDir>/run/default.conf

# Special arguments (which are parsed by this script):
#  - %scala (use Scala to run): redundant with :lang option
#  - %nojar (disable jar files)
#  - %pretend (don't actually run)
#  - %help (display help)
#  - %prof, %hprof (run profile)
#  - %q (use q to run the job)

# Additional jar files will be prepended before fig.jar and <package>.jar
def runfig2(options)
  lang = options[:lang] || 'java'

  # Which class
  package = options[:package]
  shortClassName = options[:shortClassName] || 'Main'
  className = options[:className] || (package && shortClassName && package+"."+shortClassName) or raise 'No class name'

  # Arguments to the Java program
  argv = options[:argv] || ARGV
  prependJarPaths = options[:prependJarPaths] || []

  # More variables
  javaOpts = options[:javaOpts] || getenv("JAVA_OPTS", "-mx2400m")
  baseDir = options[:baseDir] || (package && getenv(package.upcase+"_DIR"))
  assertNilOrExists(figJarPath = options[:figJarPath] || getenv("FIG_DIR")+"/fig.jar")
  assertNilOrExists(packageJarPath = options[:packageJarPath] ||
    (package && getenv("#{package.upcase}_JAR", baseDir+"/code/java/#{package}.jar")))

  if argv.member?("%help") then
    system "cat #{baseDir}/bin/usage.txt" # Print help
    exit 1
  end

  jarFiles = (prependJarPaths + [figJarPath, packageJarPath]).compact
  jarFiles = [] if argv.member?("%nojar") # Kind of a hack: disable jar files
  jarFiles.delete_if { |jarFile| not File.exists?(jarFile) } # Delete jar files that aren't there

  confFile = (shortClassName == "Main" ? "default.conf" : shortClassName+".conf")

  pretend = argv.member?('%pretend')
  lang = 'scala' if argv.member?('%scala')

  # Java opts
  javaArgs = []
  javaArgs << "-Xprof" if argv.member?("%prof")
  javaArgs << "-Xrunhprof:cpu=samples" if argv.member?("%hprof")
  #javaArgs << "-Xrunhprof:heap=all" if argv.member?("%hprof")
  javaArgs += ["-ea", javaOpts, "-server"]

  # Build arguments
  if lang == 'java'
    args = [lang] + javaArgs
  else
    args = ['env', "JAVA_OPTS=#{javaArgs.join(' ')}", lang]
  end
  args += ["-cp", jarFiles.join(":")+":"+ENV["CLASSPATH"]]
  args << className
  if argv.member?('%thunk')
    args += ['-makeThunk', '-thunkAutoQueue']
    args += ["-thunkMainClassName", className]
    args += ["-thunkJavaOpts", '---', javaArgs, '---']
    args << '-thunkUseScala' if lang == 'scala'
  end
  args += ["-create", "-monitor"] if not argv.member?('%debug')
  args += ["-useStandardExecPoolDirStrategy"]
  args += ["-jarFiles"] + jarFiles
  args << "++#{confFile}" if File.exists?(confFile)
  args = ['q'] + args if argv.member?('%q') # Run remotely using qsub
  args = ['wq'] + args if argv.member?('%wq') # Run remotely using workqueue

  args += argv.delete_if{|x| x =~ /^%/} # Remove the script options

  # Build command; quote arguments that contain spaces or other weird characters
  def quote(a); a.map { |s| s =~ /[^\w:,\.+-\/]/ ? "'"+s+"'" : s }.join(" ") end
  cmd = quote(args)

  # Execute!
  if pretend
    puts cmd
  else
    system cmd
  end
end

# For backward compatability
def runfig(package, shortClassName="Main", argv=ARGV, prependJarPaths=[], runDir=nil)
  runfig2(:package => package, :shortClassName => shortClassName,
          :argv => ARGV, :prependJarPaths => prependJarPaths, :runDir => runDir)
end

def getenv(x, defy=nil)
  # Error if x doesn't exist and default value of y doesn't exist
  y = ENV[x] || defy
  raise "Environment variable #{x} doesn't exist" unless y
  y
end
def assertNilOrExists(path)
  raise "Does not exist: '#{path}'" if path && (not File.exists?(path))
end
