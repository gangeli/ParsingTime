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
}
