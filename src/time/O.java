package time;

import org.goobs.exec.Option;

public class O {
	//--I/O
	@Option(name="train", gloss="Training range", required=true)
	public static org.goobs.utils.Range train;
	@Option(name="dev", gloss="Development range", required=true)
	public static org.goobs.utils.Range dev;
	@Option(name="test", gloss="Test range", required=true)
	public static org.goobs.utils.Range test;
	@Option(name="devTest", gloss="Report on development set")
	public static boolean devTest = false;
	@Option(name="collapseNumbers", gloss="Pre-parse numeric expressions")
	public static boolean collapseNumbers = false;
	@Option(name="bucketNumbers", gloss="Bucket --NUM-- terms into num. digits")
	public static boolean bucketNumbers = false;
	public static enum DataSource{ 
		Toy, Timebank, English }
	@Option(name="data", gloss="Data source to use")
	public static DataSource data = DataSource.Timebank;

	//--PARSING
	@Option(name="beam", gloss="Search Beam Size (memory will be 2*this)")
	public static int beam = 100000;
	public static enum SmoothingType{ none, addOne }
	@Option(name="smoothing", gloss="Smoothing type for grammar")
	public static SmoothingType smoothing = SmoothingType.none;
	@Option(name="useTime", gloss="Parse date and time both")
	public static boolean useTime = false;
	@Option(name="hardEM", gloss="Use hard rather than soft EM updates")
	public static boolean hardEM = false;
	public static enum InitType{ uniform, random }
	@Option(name="initMethod", gloss="Initialize EM to random/uniform counts")
	public static InitType initMethod = InitType.uniform;
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
	
	//--DEBUG
	@Option(name="paranoid", gloss="Paranoid correctness checks")
	public static boolean paranoid = false;
	@Option(name="useSeed", gloss="Use the same random seed")
	public static boolean useSeed = false;
	@Option(name="seed", gloss="The random seed to use")
	public static int seed = 42;
	@Option(name="goldTagFile", gloss="File to read Gold lex tags from")
	public static String goldTagFile = "goldTags";
	@Option(name="runDebug", gloss="Run a debug program")
	public static String runDebug = "none";
	@Option(name="printAllParses", gloss="Print status for all parses")
	public static boolean printAllParses = false;
	@Option(name="cacheTemporalComputations", gloss="Cache temporal computations")
	public static boolean cacheTemporalComputations = true;

}
