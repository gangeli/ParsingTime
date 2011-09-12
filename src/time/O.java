package time;

import java.util.regex.*;
import java.lang.reflect.Type;

import org.goobs.exec.Option;
import org.goobs.stats.Dirichlet;
import org.goobs.utils.Decodable;

public class O {
	public static enum Distribution{ Point, Multinomial, Gaussian }
	public static enum Scope{ Global, Local }

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
			Matcher m = DATA.matcher(encoded);
			if(!m.find()){ 
				throw new IllegalArgumentException("Invalid format: " + encoded);
			}
			this.source = DataSource.valueOf(m.group(1));
			this.begin = Integer.parseInt(m.group(2));
			this.end = Integer.parseInt(m.group(3));
			return this;
		}
		public String encode() {
			return this.source.toString()+"["+this.begin+","+this.end+"]";
		}
		@Override
		public String toString(){ return encode(); }
	}

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
	@Option(name="useTime", gloss="Parse date and time both")
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
	public static enum CkyCountType{ all, bestAll, bestRandom, bestShallow }
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

	//--TRAINING
	@Option(name="iters", gloss="Training iterations")
	public static int iters = 40;
	@Option(name="parser", gloss="Parser class to use", required=true)
	public static String parser = null;
	@Option(name="maxSearchTime", gloss="Max Iterations")
	public static int maxSearchTime = 1000000;
	@Option(name="crfTag", gloss="Use a CRF (versus PCFG) tagger")
	public static boolean crfTag = false;
	public static enum TagMethod { PCFG, CRF, GOLD }
	@Option(name="lexTagMethod", gloss="Method to tag pre-terminals")
	public static TagMethod lexTagMethod = TagMethod.PCFG;
	@Option(name="scoreBeam", gloss="Maximum possible groundings to check")
	public static int scoreBeam = 1;
	@Option(name="freeNils", gloss="NIL from a word has probability 1.0 always")
	public static boolean freeNils = false;
	//(crf)
	@Option(name="crfKBest", gloss="Number of tags to use to approx. word dist.")
	public static int crfKBest = 1000;
	@Option(name="crfFeatureFactory", gloss="FeatureFactory class for tagging")
	public static String crfFeatureFactory 
		= "edu.stanford.nlp.sequences.SuperSimpleFeatureFactory";
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
	
	//--DEBUG
	@Option(name="todoHacks", gloss="Turn on to enable quickfixes")
	public static boolean todoHacks = false;
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
	@Option(name="runDebug", gloss="Run a debug program")
	public static String runDebug = "none";
	@Option(name="printAllParses", gloss="Print status for all parses")
	public static boolean printAllParses = false;
	@Option(name="printFailures", gloss="Print parses which are not in the beam")
	public static boolean printFailures = false;
	@Option(name="cacheTemporalComputations", gloss="Cache temporal computations")
	public static boolean cacheTemporalComputations = true;

}
