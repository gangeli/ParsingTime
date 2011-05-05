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

	//--TRAINING
	@Option(name="iters", gloss="Training iterations")
	public static int iters = 40;
	@Option(name="parser", gloss="Parser class to use", required=true)
	public static String parser = null;
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
}
