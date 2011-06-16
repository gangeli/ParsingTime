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

	//--PARSING
	@Option(name="beam", gloss="Search Beam Size (memory will be 2*this)")
	public static int beam = 100000;
	@Option(name="weightedUpdate", gloss="Counts weighted by score")
	public static boolean weightedUpdate = false;
	public static enum SmoothingType{ none, addOne }
	@Option(name="smoothing", gloss="Smoothing type for grammar")
	public static SmoothingType smoothing = SmoothingType.none;
	@Option(name="useTime", gloss="Parse date and time both")
	public static boolean useTime = false;
	//<CKY>
	@Option(name="ckyPOSBackoff", gloss="Backoff for CKY from lex to pos terms")
	public static double ckyPosBackoff = 0.2;
	@Option(name="kbestCKYAlgorithm", gloss="algorithm from (Huang et. al)")
	public static int kbestCKYAlgorithm = 0;
	public static enum CkyCountType{ all, bestAll, bestRandom, bestShallow }
	@Option(name="ckyCountType", gloss="Parses to count as 'correct'")
	public static CkyCountType ckyCountType = CkyCountType.bestAll;

	//--TRAINING
	@Option(name="iters", gloss="Training iterations")
	public static int iters = 40;
	@Option(name="parser", gloss="Parser class to use", required=true)
	public static String parser = null;
	@Option(name="maxSearchTime", gloss="Max Iterations")
	public static int maxSearchTime = 1000000;
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
	
	//--DEBUG
	@Option(name="paranoid", gloss="Paranoid correctness checks")
	public static boolean paranoid = false;
	@Option(name="toy", gloss="Use the toy corpus")
	public static boolean toy = false;
}
