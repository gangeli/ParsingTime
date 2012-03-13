package time;

import java.util.regex.*;
import java.lang.reflect.Type;
import java.io.File;

import org.goobs.exec.Option;
import org.goobs.stats.Dirichlet;
import org.goobs.stats.Gaussian;
import org.goobs.util.Decodable;

public class O {
	public static enum Distribution{ Point, Multinomial, Gaussian }
	public static enum Scope{ Global, Hybrid, Local }
	public static enum RunMode{ Interpret, Detect, Console, System }

	//--Data Info
	private static Pattern DATA 
		= Pattern.compile("([a-zA-Z]+)\\[([0-9]+),([0-9]+)\\]");
	public static enum DataSource{ Toy, English, NYT }
	public static class DataInfo implements Decodable{
		public DataSource source;
		public int begin;
		public int end;
		private DataInfo(){}
		public Decodable decode(String encoded, Type[] typeParams) {
			if(encoded.trim().equalsIgnoreCase("toy")){
				this.source = DataSource.Toy;
				this.begin = 0;
				this.end = Integer.MAX_VALUE;
				return this;
			}
			Matcher m = DATA.matcher(encoded);
			if(!m.find()){ 
				throw new IllegalArgumentException("Invalid format: " + encoded);
			}
			this.source = DataSource.valueOf(m.group(1));
			this.begin = Integer.parseInt(m.group(2));
			this.end = Integer.parseInt(m.group(3));
			return this;
		}
		public String language(){
			return source.toString().toLowerCase();
		}
		public String encode() {
			return this.source.toString()+"["+this.begin+","+this.end+"]";
		}
		@Override
		public String toString(){ return encode(); }
	}
	
	@Option(name="mode", gloss="Mode to run the program in")
	public static RunMode mode = RunMode.Interpret;

	//--I/O
	@Option(name="train", gloss="Training range", required=true)
	public static DataInfo train;
	@Option(name="dev", gloss="Development range", required=true)
	public static DataInfo dev;
	@Option(name="test", gloss="Test range", required=true)
	public static DataInfo test;
	@Option(name="devTest", gloss="Report on development set")
	public static boolean devTest = false;
	@Option(name="retokenize", gloss="Tokenize on - and /")
	public static boolean retokenize = false;
	@Option(name="collapseNumbers", gloss="Pre-parse numeric expressions")
	public static boolean collapseNumbers = false;
	@Option(name="bucketNumbers", gloss="Bucket --NUM-- terms into num. digits")
	public static boolean bucketNumbers = false;
	@Option(name="ignoreCase", gloss="Ignore the case of words")
	public static boolean ignoreCase = false;

	//--PARSING
	@Option(name="beam", gloss="Search Beam Size (memory will be 2*this)")
	public static int beam = 100000;
	@Option(name="rulePrior", gloss="Dirichlet prior for CKY rules")
	public static Dirichlet<scala.Int> rulePrior = Dirichlet.ZERO();
	@Option(name="lexPrior", gloss="Dirichlet prior for CKY lexical terms")
	public static Dirichlet<scala.Int> lexPrior = Dirichlet.ZERO();
	@Option(name="nilWordPrior", gloss="Dirichlet prior for CKY NIL")
	public static Dirichlet<scala.Int> nilWordPrior = Dirichlet.ZERO();
	@Option(name="uniformRoot", gloss="Uniform probability distribution on ROOT")
	public static boolean uniformRoot = false;
	@Option(name="useTime", gloss="Parse date and time both in grammar")
	public static boolean useTime = false;
	@Option(name="hardEM", gloss="Use hard rather than soft EM updates")
	public static boolean hardEM = false;
	public static enum InitType{ uniform, random }
	@Option(name="initMethod", gloss="Initialize EM to random/uniform counts")
	public static InitType initMethod = InitType.uniform;
	@Option(name="allowPartialTime", gloss="Allow partial time as root")
	public static boolean allowPartialTime = false;
	//<CKY / EM>
	@Option(name="ckyPOSBackoff", gloss="Backoff for CKY from lex to pos terms")
	public static double ckyPosBackoff = 0.2;
	@Option(name="kbestCKYAlgorithm", gloss="algorithm from (Huang et. al)")
	public static int kbestCKYAlgorithm = 0;
	public static enum CkyCountType{ 
		all, bestAll, bestRandom, shortWithOffsetZero, mostNilsWithOffsetZero }
	@Option(name="ckyCountType", gloss="Parses to count as 'correct' for EM")
	public static CkyCountType ckyCountType = CkyCountType.bestAll;
	public static enum CkyCountNormalization{ none, uniform, 
		proportional, distribution }
	@Option(name="ckyCountNormalization",gloss="Normalize EM counts from example")
	public static CkyCountNormalization ckyCountNormalization 
		= CkyCountNormalization.none;
	@Option(name="timeDistribution", gloss="Distribution for ambiguous times")
	public static Distribution timeDistribution = Distribution.Point;
	@Option(name="timeDistributionParams", gloss="Fix distribution params")
	public static Double[] timeDistributionParams = null;
	@Option(name="timeDistributionScope", gloss="Fix distribution params")
	public static Scope timeDistributionScope = Scope.Global;
	@Option(name="gaussianSigma", gloss="Fixed sigma for the temporal gaussian")
	public static double gaussianSigma = -1.0;
	@Option(name="gaussianMuPrior", gloss="Prior for the Gaussian mean")
	public static Gaussian gaussianMuPrior = new Gaussian(0.0, 100.0);

	//--TRAINING
	//(interpretation)
	@Option(name="iters", gloss="Training iterations")
	public static int iters = 40;
	@Option(name="parser", gloss="Parser class to use", required=true)
	public static String parser = "org.goobs.nlp.CKYParser";
	@Option(name="maxSearchTime", gloss="Max Iterations")
	public static int maxSearchTime = 1000000;
	@Option(name="scoreBeam", gloss="Maximum possible groundings to check")
	public static int scoreBeam = 1;
	@Option(name="lexNils", gloss="NIL is tagged with its source word")
	public static boolean lexNils = false;
	@Option(name="includeRuleInLexProb", gloss="Lex prob = P(rule|head)*P(word|rule)")
	public static boolean includeRuleInLexProb = false;
	//(detection)
	@Option(name="crfFeatureFactory", gloss="FeatureFactory class for tagging")
	public static String crfFeatureFactory 
		= "edu.stanford.nlp.sequences.SuperSimpleFeatureFactory";
	@Option(name="maxLength", gloss="Clique size for CRF")
	public static int maxLength = 2;
	//--TESTING
	@Option(name="exactmatchThreshold", 	
		gloss="Max difference to consider exact match")
	public static int exactMatchThreshold = 60*60*24; // a day
	@Option(name="scoreGranularity", gloss="Units for dynamic score")
	public static int scoreGranularity = 60*60*24*365; // a year
	@Option(name="c_overconstraining", gloss="C_over for score")
	public static double c_overconstraining = 1.0;
	@Option(name="c_vagueness", gloss="C_vague for score")
	public static double c_vagueness = 1.0;
	@Option(name="instantAsDay", gloss="Treat a [gold] instant as a day")
	public static boolean instantAsDay = false;
	@Option(name="guessRange", gloss="Guess reference range")
	public static boolean guessRange = false;
	@Option(name="tempevalHome", gloss="Home for the original Tempeval files")
	public static File tempevalHome = new File(".");
	//--SPECIAL
	@Option(name="pruneTime", gloss="Milliseconds which should trigger pruning")
	public static double pruneTime = 100;
	@Option(name="pruneMinIndex", gloss="Minimum index for pruning")
	public static int pruneMinIndex = Integer.MAX_VALUE;
	@Option(name="includeTimeProb", gloss="P(output) = P(parse)*P(time)")
	public static boolean includeTimeProb = false;
	@Option(name="sortTimeProbInScore", gloss="Compute P(parse)*P(time) or pipeline")
	public static boolean sortTimeProbInScore = false;
	@Option(name="cannonicalShifts", gloss="<<! instead of <<")
	public static boolean cannonicalShifts = false;
	@Option(name="functionalApproximate", gloss="approx. fxn instead of primitives")
	public static boolean functionalApproximate = false;
	@Option(name="functionalUnboundedRange", gloss="toFuture and toPast fxns")
	public static boolean functionalUnboundedRange = false;
	
	//--SAVING
	@Option(name="runInterpretModel", gloss="Run the time interpretation model")
	public static boolean runInterpretModel = false;
	@Option(name="interpretModel", gloss="Location of the interpretation model")
	public static File interpretModel = null;
	
	//--DEBUG
	@Option(name="todoHacks", gloss="Turn on to enable quickfixes")
	public static boolean todoHacks = false;
	@Option(name="ignoreTimeSequences", gloss="Turn on [time]:S terms")
	public static boolean ignoreTimeSequences = false;
	@Option(name="reportK", gloss="Report parse accuracy on top K parses")
	public static int reportK = 1;
	@Option(name="paranoid", gloss="Paranoid correctness checks")
	public static boolean paranoid = false;
	@Option(name="useSeed", gloss="Use the same random seed")
	public static boolean useSeed = false;
	@Option(name="seed", gloss="The random seed to use")
	public static int seed = 42;
	@Option(name="goldTagFile", gloss="File to read Gold lex tags from")
	public static String goldTagFile = "goldTags";
	@Option(name="badTimexes", gloss="TIDs of timexes to ignore in training")
	public static String badTimexes = null;
	@Option(name="printAllParses", gloss="Print status for all parses")
	public static boolean printAllParses = false;
	@Option(name="printFailures", gloss="Print parses which are not in the beam")
	public static boolean printFailures = false;
	@Option(name="cacheTemporalComputations", gloss="Cache temporal computations")
	public static boolean cacheTemporalComputations = true;

}
