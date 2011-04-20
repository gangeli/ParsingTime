package time;

import org.goobs.exec.Option;

public class O {
	//--I/O
	@Option(name="timebank", gloss="File to read timebank from", required=true)
	public static String timebank = null;
	@Option(name="tokenizationOpts", gloss="Options for PTBTokenizer")
	public static String tokenizationOpts = null;
}
