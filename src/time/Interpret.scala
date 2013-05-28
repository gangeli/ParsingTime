package time

//(java)
import java.util.{List => JList}
import java.lang.{Integer => JInt}
import java.util.Calendar
//(scala)
import scala.collection.JavaConversions._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer
import scala.collection.immutable.Map
//(jodatime)
import org.joda.time.DateTimeZone
import org.joda.time.DateTime
//(lib)
import org.goobs.exec.Execution
import org.goobs.stanford.JavaNLP._
import org.goobs.stats._
import org.goobs.util.Indexer
import org.goobs.nlp._
import org.goobs.stanford.SerializedCoreMapDataset
import org.goobs.util.Stopwatch
import org.goobs.util.Static._
//(stanford)
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.pipeline.Annotator
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.sequences.SeqClassifierFlags
import edu.stanford.nlp.sequences.FeatureFactory
import edu.stanford.nlp.time.JodaTimeUtils
import edu.stanford.nlp.util.logging.Redwood.Util._
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.time.TimeAnnotations._
import edu.stanford.nlp.time.{Timex => StanfordTimex}
import edu.stanford.nlp.process._
import edu.stanford.nlp.pipeline._

//------------------------------------------------------------------------------
// GRAMMAR
//------------------------------------------------------------------------------
@SerialVersionUID(1L)
class TimeUnary(lambda:Option[Any=>Any],_parent:NodeType,_child:NodeType)
		extends CKYUnary(lambda,_parent,_child) {
	def this(lambda:Any=>Any,_parent:NodeType,_child:NodeType) 
		= this(Some(lambda),_parent,_child)
	def this(_parent:NodeType,_child:NodeType) = this(None,_parent,_child)

	override def equals(o:Any) = o match {
		case (a:AnyRef) => this eq a
		case _ => false
	}
}
@SerialVersionUID(1L)
class TimeBinary(lambda:Option[(Any,Any)=>Any],_parent:NodeType,
		_leftChild:NodeType,_rightChild:NodeType) 
		extends CKYBinary(lambda,_parent,_leftChild,_rightChild) {
	def this(lambda:(Any,Any)=>Any,_parent:NodeType,
			_leftChild:NodeType,_rightChild:NodeType)
		= this(Some(lambda),_parent,_leftChild,_rightChild)
	def this(_parent:NodeType,_leftChild:NodeType,_rightChild:NodeType)
		= this(None,_parent,_leftChild,_rightChild)
	override def equals(o:Any) = o match {
		case (a:AnyRef) => this eq a
		case _ => false
	}
}
@SerialVersionUID(1L)
class TimeLex(lambda:((Option[Sentence],Int)=>Any), parent:NodeType) 
		extends CKYLex(lambda,parent,NodeType.defaultFactory) {
	override def equals(o:Any) = o match {
		case (a:AnyRef) => this eq a
		case _ => false
	}
}

@SerialVersionUID(1L)
case class Grammar(index:Indexing,factory:NodeTypeFactory,lex:Lex,rules:List[GrammarRule]) {
	import lex.REF

	def removeDuplicateRules:Grammar = {
		//--De-duplicate rules
		val (newRules,signatureSet) 
			= rules.foldLeft((List[GrammarRule](),Set[(NodeType,NodeType,Option[NodeType])]())){
				case ( (acceptedRules:List[GrammarRule], seen:Set[(NodeType,NodeType,Option[NodeType])]),
				       rule:GrammarRule ) =>
			//(get parent/child signature)
			val signature:(NodeType,NodeType,Option[NodeType]) = rule match {
				case (lexR:TimeLex) => (lexR.parent,factory.WORD,None)
				case (unary:TimeUnary) => (unary.parent,unary.child,None)
				case (binary:TimeBinary) => (binary.parent,binary.leftChild,Some(binary.rightChild))
			}
			//(add if appropriate)
			if(seen(signature)){
				(acceptedRules,seen)
			} else {
				(rule :: acceptedRules, seen + signature)
			}
		}
		//--Return
		new Grammar(index,factory,lex, newRules)
	}

	def registerNumbers(nums:Array[(String,NumberType.Value,Int)]):Grammar = {
		val newRules = nums.toList.map{ case (name:String, typ:NumberType.Value, order:Int) =>
			//(create node type)
			factory.makePreterminal(name, 'num)
			//(lexical entry)
			new TimeLex((sent:Option[Sentence],i:Int) => sent.get.asNumber(i), factory(name))
				.restrict( (sent:Sentence,i:Int) => {
					val valid = {
						//((is a number))
						sent(i) == index.NUM && 
						//((and has right characteristics))
						{sent match {
							case (s:TimeSent) =>
								val theLength:Int = s.asNumber(i).toString.length-1
								val theType:NumberType.Value = s.ordinality(i)
								theType == typ && theLength == order 
							case _ => throw new IllegalStateException("Bad sentence")
						}}
					}
					valid
				})
		} ::: rules
		new Grammar(index, factory, lex, newRules)
	}
	
	def registerNumeric(
			fn:Int=>Temporal, 
			name:String, 
			eventualType:String, 
			nums:Seq[(String,NumberType.Value,Int)]):Grammar = {
		//--Make Node Types
		factory.make(eventualType, 'static)
		factory.make(name)
		//--Rules
		val newRules = {
			//(to type)
			new TimeUnary((x:Any) => x, factory(eventualType), factory(name)) ::
			nums.map{ case (numName:String, typ:NumberType.Value, order:Int) =>
				//(to term)
				new TimeUnary(fn.asInstanceOf[Any=>Any], factory(name), factory(numName))
			}.toList} :::
			//(existing rules)
			rules
		//--Return
		return new Grammar(index, factory, lex, newRules)
	}

	def registerStatic(
			term:Temporal, 
			name:String, 
			eventualType:String, 
			restrict:Option[(Sentence,Int)=>Boolean]):Grammar = {
		//--Make Node Types
		factory.make(eventualType, 'static)
		factory.makePreterminal(name)
		//--Rules
		val newRules = 
			//(lex)
			{restrict match {
				case Some(fn) => 
					new TimeLex((sent:Option[Sentence],i:Int) => term, factory(name)).restrict(
						(sent:Sentence,i:Int) => sent(i) != index.NUM && fn(sent,i))
				case None => 
					new TimeLex((sent:Option[Sentence],i:Int) => term, factory(name)).restrict(
						(sent:Sentence,i:Int) => sent(i) != index.NUM )
			}} ::
			//(to type)
			new TimeUnary((x:Any) => x, factory(eventualType), factory(name)) ::
			//(existing rules)
			rules
		//--Return
		return new Grammar(index, factory, lex, newRules)
	}
	
	def registerFunction1[F](
			fn:F,
			name:String,
			input1:String,
			output:String,
			restrict:Option[(Sentence,Int)=>Boolean] ):Grammar = {
		//--Make Node Types
		val eventualType = "f("+input1+"):"+output
		if(!factory.exists(input1)){ factory.make(input1) }
		factory.make(output, 'static)
		factory.make(eventualType, 'fn)
		factory.makePreterminal(name)
		//--Rules
		val newRules = 
				//(lex)
				new TimeLex((sent:Option[Sentence],i:Int) => fn.asInstanceOf[Any=>Temporal],factory(name))
					.restrict( (sent:Sentence,i:Int) => sent(i) != index.NUM ) ::
				//(to type)
				new TimeUnary((x:Any) => x, factory(eventualType), factory(name)) ::
				//(left apply)
				new TimeBinary(
					{(a:Any,fn:Any=>Temporal) => fn(a)}.asInstanceOf[(Any,Any)=>Any],
					factory(output),
					factory(input1),
					factory(eventualType) ) ::
				//(right apply)
				new TimeBinary(
					{(fn:Any=>Temporal,a:Any) => fn(a)}.asInstanceOf[(Any,Any)=>Any],
					factory(output),
					factory(eventualType),
					factory(input1) ) ::
				//(existing rules)
				rules
		//--Return
		new Grammar(index, factory, lex, newRules)
	}
	
	def registerFunction2[F](
			fn:F,
			name:String,
			input1:String,
			input2:String,
			output:String):Grammar = {
		//--Make Node Types
		val eventualType = "f("+input1+","+input2+"):"+output
		val intermType = "f("+input1+"):"+output
		if(!factory.exists(input1)){ factory.make(input1, 'static) }
		if(!factory.exists(input2)){ factory.make(input2, 'static) }
		factory.make(output, 'static)
		factory.make(intermType, 'fn)
		factory.make(eventualType, 'fn)
		factory.makePreterminal(name)
		//--Rules
		val newRules = {
			//(lex)
			new TimeLex((sent:Option[Sentence],i:Int) => fn.asInstanceOf[(Temporal,Temporal)=>Temporal], factory(name))
					.restrict( (sent:Sentence,i:Int) => sent(i) != index.NUM ) ::
			//(to type)
			new TimeUnary((x:Any) => x, factory(eventualType), factory(name)) ::
			//(consume arg2 from left)
			new TimeBinary(
				{(b:Temporal,fn:(Temporal,Temporal)=>Temporal) => fn(_:Temporal,b)}.asInstanceOf[(Any,Any)=>Any],
				factory(intermType),
				factory(input2),
				factory(eventualType) ) ::
			//(consume arg2 from right)
			new TimeBinary(
				{(fn:(Temporal,Temporal)=>Temporal,b:Temporal) => fn(_:Temporal,b)}.asInstanceOf[(Any,Any)=>Any],
				factory(intermType),
				factory(eventualType),
				factory(input2) ) :: Nil} :::
			{if(input2 == "Range"){
				//(consume arg1 from the left with ref)
				new TimeBinary(
					{(a:Temporal,fn:(Temporal,Temporal)=>Temporal) => fn(a,REF)}.asInstanceOf[(Any,Any)=>Any],
					factory(output),
					factory(input1),
					factory(eventualType) ) ::
				//(consume arg1 from the right with ref)
				new TimeBinary(
					{(fn:(Temporal,Temporal)=>Temporal,a:Temporal) => fn(a,REF)}.asInstanceOf[(Any,Any)=>Any],
					factory(output),
					factory(eventualType),
					factory(input1) ) :: Nil
			} else {
				List[TimeBinary]()
			}} ::: 
			{if(input1 == "Range"){
				//(consume arg2 from the left with ref)
				new TimeBinary(
					{(b:Temporal,fn:(Temporal,Temporal)=>Temporal) => fn(REF,b)}.asInstanceOf[(Any,Any)=>Any],
					factory(output),
					factory(input2),
					factory(eventualType) ) ::
				//(consume arg2 from the right with ref)
				new TimeBinary(
					{(fn:(Temporal,Temporal)=>Temporal,b:Temporal) => fn(REF,b)}.asInstanceOf[(Any,Any)=>Any],
					factory(output),
					factory(eventualType),
					factory(input2) ) :: Nil
			} else {
				List[TimeBinary]()
			}} ::: 
			//(existing rules)
			rules
		//--Return
		new Grammar(index, factory, lex, newRules)
	}

	def registerBinary[F](fn:F, output:String, input1:String, input2:String) = {
		val newRules = 
			new TimeBinary(
				fn.asInstanceOf[(Any,Any)=>Any], 
				factory(output), 
				factory(input1), 
				factory(input2) ) ::
			rules
		new Grammar(index, factory, lex, newRules)
	}

	def mkNils(nums:Seq[(String,NumberType.Value,Int)]):Grammar = {
		//--Node Types
		val newRules = {if(O.lexNils){
			//(non-numeric nils)
			(0 until index.W).filter( _ != index.NUM ).map{ (w:Int) =>
				(factory.makePreterminal("nil-"+index.w2str(w), 'nil), w, None)
			}.toList :::
			//(numeric nils)
			nums.map{ case (name:String,typ:NumberType.Value,order:Int) =>
				(factory.make("nil-"+name, 'nil, 'nilnum), -1, Some(name))
			}.toList
		} else {
			//(single nil -- unlexicalized)
			List[(NodeType,Int,Option[String])]( 
				(factory.makePreterminal("nil", 'nil), -1, None) )
		//--Rules
		}}.flatMap{ case (nil:NodeType, w:Int, numName:Option[String]) =>
			{if(numName.isDefined){
				//(intro for number)
				new TimeUnary((x:Any) => new Grammar.NIL(), nil, factory(numName.get))
			} else {
				//(intro for word)
				new TimeLex((sent:Option[Sentence],i:Int) => new Grammar.NIL(), nil)
					.restrict((sent:Sentence,i:Int) => {
						w < 0 || sent(i) == w
					} )
			}} ::
			//(combination)
			new TimeBinary(	(a:Any,term:Any) => term, factory("Sequence"), nil, factory("Sequence") ) ::
			new TimeBinary(	(term:Any,a:Any) => term, factory("Sequence"), factory("Sequence"), nil ) ::
			new TimeBinary(	(a:Any,term:Any) => term, factory("Range"),    nil, factory("Range") ) ::
			new TimeBinary(	(term:Any,a:Any) => term, factory("Range"),    factory("Range"), nil ) ::
			new TimeBinary(	(a:Any,term:Any) => term, factory("Duration"), nil, factory("Duration") ) ::
			new TimeBinary(	(term:Any,a:Any) => term, factory("Duration"), factory("Duration"), nil ) :: 
			Nil
		} ::: rules
		//(return)
		new Grammar(index, factory, lex, newRules)
	}

	def root:Grammar = {
		val newRules =
			new TimeUnary((x:Any) => x, factory.ROOT, factory("Sequence") ) ::
			new TimeUnary((x:Any) => x, factory.ROOT, factory("Duration") ) ::
			new TimeUnary((x:Any) => x, factory.ROOT, factory("Range")    ) ::
			rules
		new Grammar(index, factory, lex, newRules)
	}

	def r2str(r:CKYRule):String = r.parent.toString// + "("+System.identityHashCode(r)+")"
	def lexPrior:NodeType=>Prior[Int,Multinomial[Int]] 
		= (parent:NodeType) => {
			if(O.lexNils && parent.flag('nil) && !parent.flag('nilnum)){
				if(O.nilPrior == O.NilPrior.lex){
					//(case: treat like normal lex term)
					O.lexPrior
				} else if(O.nilPrior == O.NilPrior.uniform){
					//(case: enforce uniform)
					Uniform.mkPrior[Int]()
				} else if(O.nilPrior == O.NilPrior.free){
					//(case: free nils)
					val w = index.str2wTest(parent.toString.substring(4))
					Dirichlet.fromMap(
						Map( w -> 1.0 ).map{ case (x,y) => (x, y.asInstanceOf[java.lang.Double]) } )
				} else {
					throw fail("Invalid nil prior handling case: " + O.nilPrior)
				}
			} else {
				O.lexPrior
			}
		}
	
	def rulePrior:NodeType=>Prior[Int,Multinomial[Int]] 
		= (parent:NodeType) => O.rulePrior

	/**
		Returns a set of [lowercase] words which the parser believes to be reasonable
		lexical entries
	*/
	def lexWords(parser:CKYParser,probThreshold:Double=0.01):Set[String] = {
		val notNil = rules.flatMap{ (rule:GrammarRule) =>
			rule match {
				case (lex:TimeLex) =>
					if(!lex.parent.flag('nil) && !lex.parent.flag('num)){
						parser.sortedLexProbs(lex)
							.takeWhile{ case (p:Double,w:Int) => p >= probThreshold }
							.map{ case (p:Double,w:Int) => 
								index.w2str(w).toLowerCase 
							}
					} else {
						List[String]()
					}
				case _ =>
					List[String]()
			}
		}.toSet
		notNil
	}
	
//	/**
//		Returns a set of [lowercase] words which the parser believes to be reasonable
//		lexical entries
//	*/
//	def lexWords(parser:CKYParser):Set[String] = {
//		(0 until index.W).map{ (w:Int) =>
//			if(w != index.NUM && w != index.UNK){
//				//(parse)
//				val tree = parser.parse(TimeSent(Array[Int](w),
//					Array[Int](0),Array[Int](Int.MinValue),Array[NumberType.Value](NumberType.NONE),index))
//				//(prob)
//				val prob = math.exp(tree.logProb)
//				if(!tree.lexRules(0).parent.flag('nil) && prob > 0.001){
//					//(case: parsed to not nil)
//					log(FORCE,G.df.format(prob) + " " + index.w2str(w).toLowerCase )
//					Some(index.w2str(w).toLowerCase)
//				} else {
//					//(case: nil parse)
//					None
//				}
//			} else {
//				//(case: invalid word)
//				None
//			}
//		}.filter{ _.isDefined }.map{ _.get }.toSet
//	}


}

object Grammar {
	//--Values
	case class NIL()
	val DOW_STR = Array[String]("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
	val MOY_STR = Array[String]("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	val QOY_STR = Array[String]("Q1","Q2","Q3","Q4")
	val SEASON_STR = Array[String]("SP","SU","FA","WI")
	val TOD_STR = Array[String]("MO","AF","EV","NI")
	def getNumFromOrder(order:Int,typ:NumberType.Value):Symbol = {
		Symbol("NUM*$10^"+order+"$:"+typ.toString)
	}
	def getNum(num:Int,typ:NumberType.Value):Symbol = {
		getNumFromOrder(num.toString.length-1,typ)
	}
	//--Init Function
	val nums:Array[(String,NumberType.Value,Int)] = {
		(0 to 3).map{ (order:Int) =>
			NumberType.values.map{ (typ:NumberType.Value) =>
				("NUM*$10^"+order+"$:"+typ.toString, typ, order)
			}
		}.flatten.toArray
	}

	//--Create Grammar
	def apply(index:Indexing, lex:Lex, factory:NodeTypeFactory):Grammar = {
		import lex._
		val duration:String = "Duration";
		val range:String = "Range";
		val sequence:String = "Sequence";

		var grammar = new Grammar(index, factory, lex, Nil).registerNumbers(nums)

		//--Static
		//(ranges)
		grammar = grammar
			.registerStatic(PAST,      "past",      range, None)
			.registerStatic(FUTURE,    "future",    range, None)
			.registerStatic(YESTERDAY, "yesterday", range, None)
			.registerStatic(TOMORROW,  "tomorrow",  range, None)
			.registerStatic(TODAY,     "today",     range, None)
			.registerStatic(REF,       "ref",       range, None)
		//(durations)
		if(O.useTime){
			grammar = grammar
				.registerStatic(ASEC,    "asec",      duration, None)
				.registerStatic(AMIN,    "amin",      duration, None)
				.registerStatic(AHOUR,   "ahour",     duration, None)
		}
		grammar = grammar
			.registerStatic(ADAY,      "aday",      duration, None)
			.registerStatic(AWEEK,     "aweek",     duration, None)
			.registerStatic(AMONTH,    "amonth",    duration, None)
			.registerStatic(AQUARTER,  "aquarter",  duration, None)
			.registerStatic(AHALFYEAR, "ahalfyear", duration, None)
			.registerStatic(AYEAR,     "ayear",     duration, None)
			.registerStatic(ADECADE,   "adecade",   duration, None)
			.registerStatic(ACENTURY,  "acentury",  duration, None)
		//(approximate durations)
		if(!O.functionalApproximate){
			if(O.useTime){
				grammar = grammar
					.registerStatic(~ASEC,    "~asec",      duration, None)
					.registerStatic(~AMIN,    "~amin",      duration, None)
					.registerStatic(~AHOUR,   "~ahour",     duration, None)
			}
			grammar = grammar
				.registerStatic(~ADAY,      "~aday",      duration, None)
				.registerStatic(~AWEEK,     "~aweek",     duration, None)
				.registerStatic(~AMONTH,    "~amonth",    duration, None)
				.registerStatic(~AQUARTER,  "~aquarter",  duration, None)
				.registerStatic(~AHALFYEAR, "~ahalfyear", duration, None)
				.registerStatic(~AYEAR,     "~ayear",     duration, None)
				.registerStatic(~ADECADE,   "~adecade",   duration, None)
				.registerStatic(~ACENTURY,  "~acentury",  duration, None)
		}
		//(sequences -- dense)
		if(O.useTime && !O.ignoreTimeSequences){
			grammar = grammar
				.registerStatic(SEC,    "sec",      sequence, None)
				.registerStatic(MIN,    "min",      sequence, None)
				.registerStatic(HOUR,   "hour",     sequence, None)
		}
		grammar = grammar
			.registerStatic(DAY,      "day",      sequence, None)
			.registerStatic(WEEK,     "week",     sequence, None)
			.registerStatic(MONTH,    "month",    sequence, None)
			.registerStatic(QUARTER,  "quarter",  sequence, None)
			.registerStatic(HALFYEAR, "halfyear", sequence, None)
			.registerStatic(YEAR,     "year",     sequence, None)
		//(sequences -- sparse)
		(1 to 7).foreach{ (dow:Int) =>	
			grammar = grammar.registerStatic(DOW(dow), DOW_STR(dow-1), sequence, None) 
		}
		(1 to 12).foreach{ (moy:Int) =>	
			grammar = grammar.registerStatic(MOY(moy), MOY_STR(moy-1), sequence, None) 
		}
		(1 to 4).foreach{ (season:Int) =>	
			grammar = grammar.registerStatic(SEASON(season), SEASON_STR(season-1), sequence, None) 
		}
		if(O.useTime){
			(1 to 4).foreach{ (time:Int) =>	
				grammar = grammar.registerStatic(TOD(time), TOD_STR(time-1), sequence, None) 
			}
		}
		//(sequences -- indexed -- e.g., [May] *17th*)
		def nthFn(seq:Array[_<:Sequence]) = 
		{(n:Int) => if(n < 0 || n >= seq.length) new NoTime else seq(n)}.asInstanceOf[Any=>Temporal]
		if(O.useTime){
			grammar = grammar
				.registerNumeric(nthFn(MOH), "moh(n)", sequence, nums) 
				.registerNumeric(nthFn(HOD), "hod(n)", sequence, nums) 
			}
		grammar = grammar
			.registerNumeric(nthFn(DOM),     "dom(n)",     sequence, nums) 
			.registerNumeric(nthFn(MOY),     "moy(n)",     sequence, nums) 
			.registerNumeric(nthFn(YOC),     "yoc(n)",     sequence, nums) 
			.registerNumeric(nthFn(DOC),     "doc(n)",     sequence, nums) 
			.registerNumeric(nthFn(YOD),     "yod(n)",     sequence, nums) 
			.registerNumeric(THEYEAR(_:Int), "year(n)",    range,    nums) 
			.registerNumeric(CENTURY(_:Int), "century(n)", range,    nums) 

		//--Function1
		//(indexing function -- e.g., [3rd] *quarter*)
		nums.zipWithIndex.foreach{
				case ((numName:String, numType:NumberType.Value, magnitude:Int), i:Int) =>
			grammar = grammar
				//(usual)
				.registerFunction1(nthFn(DOW),      "dow(nth)",         numName, sequence, None)
				.registerFunction1(nthFn(WOM),      "wom(nth)",         numName, sequence, None)
				.registerFunction1(nthFn(WOY),      "woy(nth)",         numName, sequence, None)
				.registerFunction1(nthFn(MOY),      "moy(nth)",         numName, sequence, None)
				.registerFunction1(nthFn(QOY),      "qoy(nth)",         numName, sequence, None)
				//(day of week of month)
				.registerFunction1(nthFn(DOWOM(1)), DOW_STR(0)+"(nth)", numName, sequence, None)
				.registerFunction1(nthFn(DOWOM(2)), DOW_STR(1)+"(nth)", numName, sequence, None)
				.registerFunction1(nthFn(DOWOM(3)), DOW_STR(2)+"(nth)", numName, sequence, None)
				.registerFunction1(nthFn(DOWOM(4)), DOW_STR(3)+"(nth)", numName, sequence, None)
				.registerFunction1(nthFn(DOWOM(5)), DOW_STR(4)+"(nth)", numName, sequence, None)
				.registerFunction1(nthFn(DOWOM(6)), DOW_STR(5)+"(nth)", numName, sequence, None)
				.registerFunction1(nthFn(DOWOM(7)), DOW_STR(6)+"(nth)", numName, sequence, None)
		}
		//(functions)
		grammar = grammar
			.registerFunction1(move(_:Sequence,-1L), "moveLeft1",  sequence, sequence, None)
			.registerFunction1(move(_:Sequence, 1L), "moveRight1", sequence, sequence, None)
		if(O.functionalApproximate){
			grammar = grammar
				.registerFunction1(fuzzify,            "fuzzify",    duration, duration, None)
		}
		if(O.functionalUnboundedRange){
			grammar = grammar
				.registerFunction1(toPast,             "toPast",     duration, duration, None)
				.registerFunction1(toFuture,           "toFuture",   duration, duration, None)
		}
		
		//--Function2
		grammar = grammar
			.registerFunction2(if(O.cannonicalShifts) cannonicalLeft else shiftLeft, 
				                              "shiftLeft",   range,    duration, range)
			.registerFunction2(if(O.cannonicalShifts) cannonicalLeft else shiftLeft, 
				                              "shiftLeft",   sequence, duration, sequence)
			.registerFunction2(if(O.cannonicalShifts) cannonicalRight else shiftRight, 
				                              "shiftRight",  range,    duration, range)
			.registerFunction2(if(O.cannonicalShifts) cannonicalRight else shiftRight, 
				                              "shiftRight",  sequence, duration, sequence)
			.registerFunction2(shrinkBegin, "shrinkBegin", range,    duration, range)
			.registerFunction2(shrinkBegin, "shrinkBegin", sequence, duration, sequence)
			.registerFunction2(shrinkEnd,   "shrinkEnd",   range,    duration, range)
			.registerFunction2(shrinkEnd,   "shrinkEnd",   sequence, duration, sequence)
			.registerFunction2(catLeft,     "catLeft",     range,    duration, range)
			.registerFunction2(catRight,    "catRight",    range,    duration, range)

		//--Special
		//(intersect)
		grammar = grammar
			.registerBinary((a:Sequence,b:Sequence) => a ^ b, sequence, sequence, sequence)
			.registerBinary((a:Sequence,b:Range) => a ^ b,    range,    sequence, range)
			.registerBinary((a:Range,b:Sequence) => a ^ b,    range,    range,    sequence)
			.registerBinary((a:Range,b:Range) => a ^ b,       range,    range,    range)
		//(multiply)
		nums.foreach{ case (numName:String, numType:NumberType.Value, magnitude:Int) =>
			grammar = grammar
				.registerBinary((n:Int,d:Duration) => d * n,    duration, numName,  duration)
				.registerBinary((d:Duration,n:Int) => d * n,    duration, duration, numName)
		}

		//--Return
		return grammar.removeDuplicateRules.mkNils(nums).root
	}
}


//------------------------------------------------------------------------------
// GROUNDING DATA
//------------------------------------------------------------------------------
class GroundingData(impl:TimeDataset,train:Boolean,index:Indexing
		) extends DataStore[(TimeSent,Temporal,Time)] with Iterable[Annotation] {
	def name:String = "Grounding-"+{if(train) "train" else "eval"}
	def eachExample(i:Int):Iterable[(TimeSent,Temporal,Time)] 
		= impl.goldSpans(train,index)
	def iterator:Iterator[Annotation] 
		= impl.data.iterator.map{ _.impl.asInstanceOf[Annotation] }
}

//------------------------------------------------------------------------------
// GROUNDING TASK
//------------------------------------------------------------------------------
trait TemporalTask {
	def run:Unit
}
@SerialVersionUID(2L)
case class TreeTime(
		parser:CKYParser,
		grammar:Grammar,
		lex:Lex,
		crf:Option[CRFDetector],
		crfIndex:Option[Indexing]
		) extends Annotator {
	assert(!crf.isDefined || crfIndex.isDefined, "CRF without index")
	assert(!crfIndex.isDefined || crf.isDefined, "CRF index without CRF")
	assert(!crf.isDefined || crf.get.index == crfIndex.get, "Indices mismatch")

	def this(parser:CKYParser,grammar:Grammar,lex:Lex)
		= this(parser,grammar,lex,None,None)

	def index:Indexing = grammar.index

	def addDetector(detector:CRFDetector,detectorIndex:Indexing):TreeTime = {
		new TreeTime(parser,grammar,lex,Some(detector),Some(detectorIndex))
	}

  lazy val pipeline:StanfordCoreNLP = {
    val props = new java.util.Properties
    props.setProperty("annotators", "tokenize, ssplit, pos")
    new StanfordCoreNLP(props)
  }

  def parseTimex(sent:String, groundTimeAsMillis:Long):String = {
    val ann = new Annotation(sent)
    pipeline.annotate(ann)
    val tokens:Buffer[CoreLabel] = ann.get[JList[CoreMap],SentencesAnnotation](classOf[SentencesAnnotation]).get(0).get[JList[CoreLabel],TokensAnnotation](classOf[TokensAnnotation])
    val timeSent:TimeSent = DataLib.mkTimeSent(tokens, index, false)
    parseTimex(timeSent, Time(new DateTime(groundTimeAsMillis)))
  }

  def parseTimex(sent:TimeSent, ground:Time):String = {
		val (time,original,logProb) = parse(sent,ground)
    toStanfordTimex(time, original).value
  }

	def parse(sent:TimeSent,ground:Time):(Temporal,Temporal,Double) = {
		//(run parser)
		var time:Temporal = new NoTime
		var originalTime:Temporal = new NoTime
		var logProb:Double = Double.NegativeInfinity
		var beam:Int = 1
		while(time.isInstanceOf[NoTime] && beam <= 128){
			//((parse))
  		val parses:Array[EvalTree[Any]] = parser.parse(sent.reIndex(index),beam)
			//((get candidate))
			val (t,ot,lP) = parses.slice(beam/2-1,beam)
					.foldLeft((
						(new NoTime).asInstanceOf[Temporal],
						(new NoTime).asInstanceOf[Temporal],
						Double.NegativeInfinity)){ 
					case ((soFar:Temporal,orig:Temporal,lProb:Double),parse:EvalTree[Any]) =>
				if(soFar.isInstanceOf[NoTime]){
					val rawTime = parse.evaluate.asInstanceOf[Temporal]
					val timeDist = rawTime.distribution(Range(ground,ground))
					if(timeDist.hasNext){
						val (evaluated,timeProb,offset) = timeDist.next
						log("parse: " + parse.asParseString(index.w2str(_),grammar.r2str(_)) )
						(evaluated, rawTime, parse.logProb + math.log(timeProb)) //<--found a parse
					} else {
						(soFar,orig,lProb) //<--evaluated to NoTime
					}
				} else {
					(soFar,orig,lProb) //<--early exit
				}
			}
			//((set variables))
			time = t
			originalTime = ot
			logProb = lP
			//((increment beam))
			beam *= 2
		}
		(time,originalTime,logProb)
	}

	private def toStanfordTimex(time:Temporal,original:Temporal):StanfordTimex = {
		//(get timex data)
		import JodaTimeUtils._
		val (typ,value) = time match {
			case (n:NoTime) => 
				("MISS","?")
			case (gr:GroundedRange) => 
				if(original.isInstanceOf[UngroundedRange] &&
						original.asInstanceOf[UngroundedRange].isRef){
					("DATE","PRESENT_REF")
				} else {
					val opts = new JodaTimeUtils.ConversionOptions
					opts.forceDate = false
					("DATE",timexDateValue(gr.begin.base,gr.end.base,opts))
				}
			case (d:GroundedDuration) => 
				val opts = new JodaTimeUtils.ConversionOptions
				opts.approximate = false
//				opts.forceUnits = d.timexUnits
				opts.forceUnits = Array[String]("L","C","Y","M")
				("DURATION",timexDurationValue(d.base,opts))
			case (d:FuzzyDuration) => 
				val opts = new JodaTimeUtils.ConversionOptions
//				opts.forceUnits = d.timexUnits
				opts.forceUnits = Array[String]("L","C","Y","M")
				opts.approximate = true
				("DURATION",timexDurationValue(d.interval.base,opts))
			case (t:Time) => ("TIME",timexTimeValue(t.base))
			case _ => throw new IllegalStateException("Unknown time: " + time.getClass)
		}
		//(create timex)
		new StanfordTimex(typ,value)
	}

	private def annotateKnownSpan(span:JList[CoreLabel],ground:Time) = {
		//--Parse
		//(create sentence)
		val sent = DataLib.mkTimeSent(span,index,false)
		//(parse)
		val (time,original,logProb) = parse(sent,ground)
		if( math.exp(logProb/(3*sent.length)) >= O.interpretThreshold ){
			//--Annotate
			val timex = toStanfordTimex(time,original)
			log("annotated "+sent+" as "+timex)
			//(annotate)
			span.foreach{ case (tok:CoreLabel) =>
				tok.set(classOf[TimexAnnotation], timex)
			}
		}
	}
	
	private def annotateSentenceCRF(tokens:JList[CoreLabel], ground:Time) = {
		//--Run CRF
		val tokensArray:Array[CoreLabel] = tokens.map{ x => x }.toArray
		val sent = crf.get.corelabels2timeSent(tokensArray)
		val gloss:Array[String] = tokens.map{ _.word }.toArray
		assert(gloss.length == sent.length, "gloss mismatch")
		val times:Array[DetectedTime] = crf.get.findTimes(sent,
			(s:TimeSent) => {
				crfIndex.map{ (crfInd:Indexing) =>
					parse(s.reIndex(crfInd),ground)
				}
			},
			parser.chart(sent.reIndex(crfIndex.get)),
//			Map[(NodeType,Int,Int),Double](),
			gloss)
		//--Annotate
		times.foreach{ case DetectedTime(begin,end,timeInfo) =>
			val (time,original,logProb) = timeInfo.get
			val timex = toStanfordTimex(time,original)
			log("detected ["+begin+"-"+end+") " + 
				tokens.slice(begin,end).map{ _.word }.mkString(" ") + 
				" as " + timex)
			(begin until end).foreach{ (i:Int) =>
				tokens.get(i).set(classOf[TimexAnnotation], timex)
			}
		}
	}
	
	private def annotateSentence(tokens:JList[CoreLabel], ground:Time) = {
		//--Parse
		//(create sentence)
		val sent = DataLib.mkTimeSent(tokens,index,false,9999) //a teeny bit hacky...
		log(FORCE,"|sent|="+sent.length)
		//(get chart)
		val chart = parser.chart(sent,0,Int.MaxValue, List(grammar.factory.ROOT))
		//--Detect
		chart
			//(calculate score)
			.map{ case ((n:NodeType,s:Int,e:Int),value:Double) => 
				val len = e-s
				val numRules = (3*len).toDouble
				((s,e), math.exp(value / numRules)) //parse_score = (**ave_rule_score**)^(num_rules)
			}.toArray
			//(sort by score descendinascending)
			.sortWith{ case (((bA:Int,eA:Int),sA:Double),((bB:Int,eB:Int),sB:Double)) =>
			 	sA > sB
			//(take top elements)
			}.takeWhile{ case (key:(Int,Int),score:Double) => score > O.detectThreshold }
			//(tag timex)
			.reverse
			.foreach{ case ((begin:Int,end:Int),score:Double) =>
				//(parse timex)
				val (time,original,logProb) = parse(sent.slice(begin,end),ground)
				val timex = toStanfordTimex(time,original)
				//(annotate)
				log(FORCE,"detected ["+begin+"-"+end+") " + 
					tokens.slice(begin,end).map{ _.word }.mkString(" ") + 
					" as " + timex)
				(begin until end).foreach{ (i:Int) =>
					tokens.get(i).set(classOf[TimexAnnotation], timex)
				}
			}
	}
	
	//<<annotator overrides>>
	override def annotate(ann:Annotation){ //DESTRUCTIVE! (todo prolly shouldn't be)
		//--Overhead
		//(ground)
		val ground = new Time(new DateTime(ann
				.get[Calendar,CalendarAnnotation](classOf[CalendarAnnotation])
				.getTimeInMillis ))
		//(annotate)
		DataLib.retokenize(ann)
		DataLib.normalizeNumbers(ann)
		//--Cycle Sentences
		ann
				.get[JList[CoreMap],SentencesAnnotation](classOf[SentencesAnnotation])
				.foreach{ (sent:CoreMap) =>
			//(variables)
			val tokens = sent
					.get[JList[CoreLabel],TokensAnnotation](classOf[TokensAnnotation])
			//--Annotate
			if(crf.isDefined){
				if(O.detectMode == O.DetectMode.crf){
					annotateSentenceCRF(tokens,ground)
				} else {
					annotateSentence(tokens,ground)
				}
			} else {
				annotateKnownSpan(tokens,ground)
			}
		}
	}
}



class InterpretationTask extends TemporalTask {
	//--Initialize JodaTime
	log("JodaTime settings")
	DateTimeZone.setDefault(DateTimeZone.UTC);
	//--Create Data
	forceTrack("loading dataset")
	val (data,index) = {
		//(raw dataset)
		val rawDataset = new TimeDataset(new SerializedCoreMapDataset(
			"aux/coremap/tempeval2-english-retok-numbers"
			))
		val eval = if(O.devTest) O.dev else O.test
		//(create data)
		if(O.train.source == O.DataSource.Toy || eval.source == O.DataSource.Toy){
		  (ToyData.STANDARD,ToyData.index)
		} else {
			val index = Indexing()
		  (TimeData(
				new GroundingData( rawDataset.slice(O.train.begin,O.train.end),
					true, index),
				new GroundingData( rawDataset.slice(eval.begin,eval.end),
					false, index)
			),
			index)
		}
	}
	forceTrack("NOOP loop")
	data.noopLoop
	endTrack("NOOP loop")
	endTrack("loading dataset")
	//--Create Parser
	startTrack("Creating Grammar")
	val lex = new Lex
	import lex._
	val grammar = Grammar(index,lex,NodeType.defaultFactory)
//	grammar.rules.foreach{ log(FORCE,_) }
	endTrack("Creating Grammar")

	//<<scoring>>
	case class GoodOutput(tree:EvalTree[Any],value:Temporal,
			offset:Long,prob:Double,ground:Time,sent:TimeSent) {
		assert(prob >= 0 && prob <= 1.0, "Invalid probability: " + prob)
		def logProb:Double = math.log(prob)
		def normalize(total:Double,count:Int):GoodOutput = {
			if(total == 0.0 || O.ckyCountNormalization == O.CkyCountNormalization.uniform){
				//(case: uniform)
				GoodOutput(tree,value,offset,1.0/count.asInstanceOf[Double],ground,sent)
			} else if(O.ckyCountNormalization == O.CkyCountNormalization.distribution) {	
				//(case: distribution)
				GoodOutput(tree,value,offset,prob/total,ground,sent)
			} else {
				//(case: no normalization)
				this
			}
		}
	}
	case class CompareElem(offset:Int,diff:(Duration,Duration),prob:Double)
	case class ScoreElem(index:Int,offset:Int,diff:(Duration,Duration),
			logProb:Double, temporal:Temporal){
		def exact:Boolean = { U.sumDiff(diff) < O.exactMatchThreshold }
	}
	
	def compare(guessRaw:Temporal, gold:Temporal,ground:GroundedRange
			):Iterator[CompareElem] = {
		val INF = (Duration.INFINITE,Duration.INFINITE)
		def diff(gold:Temporal,guess:Temporal,second:Boolean):(Duration,Duration) 
				= (gold,guess) match {
			//--Immediate Invalids
			//(case: unks)
			case (gold:UnkTime,guess:Temporal) => INF
			case (gold:Temporal,guess:NoTime) => INF
			case (gold:Temporal,guess:Sequence) =>
				throw fail("Distribution returned a sequence: " + guess)
			case (gold:Sequence,guess:Temporal) =>
				throw fail("Gold is a sequence: " + gold)
			//(case: type errors)
			case (gold:FuzzyDuration,guess:GroundedDuration) => INF
			case (gold:GroundedDuration,guess:FuzzyDuration) => INF
			//--Valid
			//(case: durations)
			case (gold:FuzzyDuration,guess:FuzzyDuration) => 
				if(gold.largestUnit == guess.largestUnit){(Duration.ZERO,Duration.ZERO)}
				else{ INF }
			case (gold:GroundedDuration,guess:GroundedDuration) => 
				(guess-gold,Duration.ZERO)
			//(case: grounded ranges)
			case (gold:GroundedRange,guess:GroundedRange) => 
				if(guess.norm.seconds == 0 && gold.norm.seconds == 0){
					(Duration.ZERO,guess.begin-gold.begin) //case: instant
				} else if(O.instantAsDay && gold.norm.seconds == 0){
					(guess.begin-gold.begin,guess.end-(gold.end+DAY))
				} else {
					assert(!guess.begin.equals(Time.DAWN_OF) || guess.begin==Time.DAWN_OF)
					assert(!guess.end.equals(Time.END_OF) || guess.end==Time.END_OF)
					if(guess.begin == Time.DAWN_OF && gold.begin == Time.DAWN_OF){
						(Duration.ZERO,Duration.ZERO) //case: past
					} else if(guess.end == Time.END_OF && gold.end == Time.END_OF){
						(Duration.ZERO,Duration.ZERO) //case: future
					} else if(guess.begin == Time.DAWN_OF && gold.begin != Time.DAWN_OF){
						INF //case: beginning is neg_infinity
					} else if(guess.end == Time.END_OF && gold.end != Time.END_OF){
						INF //case: end is pos_infinity
					} else {
						(guess.begin-gold.begin,guess.end-gold.end) //case: can subtract
					}
				}
			//--Possibly Valid
			//(case: backoffs)
			case (gold:Range,guess:GroundedRange) => 
				if(second){ INF }
				else { val gr:GroundedRange = gold(ground); diff(gr,guess,true) }
			case (gold:PartialTime,guess:Temporal) =>
				if(second){ INF }
				else { val gr:GroundedRange = gold(ground); diff(gr,guess,true) }
			//--Not Valid
			//(case: didn't catch above)
			case (gold:Duration,guess:Duration) => throw fail("case: 2 durations")
			case (gold:Range,guess:Range) => throw fail("case: 2 durations")
			//(case: type error)
			case (gold:Duration,guess:Temporal) =>  INF
			case (gold:Range,guess:Temporal) => INF
			//(case: default fail)
			case _ => throw fail("Unk (invalid?) case: gold "+gold+" guess "+guess)
		}
		//--Map Iterator
		val rtn = guessRaw.distribution(ground).map{
				case (guess:Temporal,prob:Double,offset:Long) =>
			//(get diff)
			val d = diff(gold,guess,false)
//			//(check timex consistency)
//			if(U.sumDiff(d) > O.exactMatchThreshold){
//				import edu.stanford.nlp.time.JodaTimeUtils._
//				val (tGold, tGuess) = (gold,guess) match {
//					case (a:UnkTime, b:Temporal) => {("not", "equal")}
//					case (a:Temporal, b:NoTime) => {("not", "equal")}
//					case (a:GroundedRange,b:GroundedRange) => {
//						(timexDateValue(a.begin.base, a.end.base),
//							timexDateValue(b.begin.base,b.end.base))
//					}
//					case (a:FuzzyDuration,b:FuzzyDuration) => {
//						(timexDurationValue(a.interval.base,true),
//							timexDurationValue(b.interval.base,true))
//					}
//					case (a:GroundedDuration,b:GroundedDuration) => {
//						(timexDurationValue(a.interval.base),
//								timexDurationValue(b.interval.base))
//					}
//					case _ => ("not","equal")
//				}
//				if(tGold.equals(tGuess)){
//					log("Timexes match but " +
//						"difference is nonzero: gold="+tGold+" guess="+tGuess+
//						"  myGuess="+guess+"  inferredGold="+gold+" (diff="+d+") :: ")
//				}
//			}
//			//(debug)
//			assert(O.timeDistribution != O.Distribution.Point || //TODO enable me
//				offset == 0L ||
//				prob == 0.0,
//				"Time returned distribution when it shouldn't have: " 
//					+ guessRaw + " (offset=" + offset + ") [prob=" + prob + "]")
			//(return)
			CompareElem(offset.toInt,d,prob)
		}
		rtn
	}

	def filterCorrect(correct:Array[GoodOutput]):Iterable[GoodOutput] = {
		//--Utility Functions
		def countNonNils(tree:ParseTree,sent:Sentence):(Int,Int) = {
			var words = List[Int]()
			var tags = List[CKYUnary]()
			tree.traverse( 
				(rule:CKYRule) => {}, 
				(rule:CKYUnary,w:Int) => { 
					tags = rule :: tags 
					words = w :: words 
				} )
			val nonNilCount:Int = tags.filter(_.parent.flag('nil)).length
			val trimmedLength:Int = words.zip(tags)
				.dropWhile{ case (w,t) => t.parent.flag('nil)}
				.reverse
				.dropWhile{ case (w,t) => t.parent.flag('nil)}
				.map{_._1}
				.length
			(trimmedLength,nonNilCount)
		}
		def trim(tree:ParseTree,sent:Sentence):Array[Int] = {
			var words = List[Int]()
			var tags = List[CKYRule]()
			tree.traverse( 
				(rule:CKYRule) => {}, 
				(rule:CKYUnary,w:Int) => { 
					tags = rule :: tags 
					words = w :: words 
				} )
			words.zip(tags)
				.dropWhile{ case (w,t) => t.parent.flag('nil)}
				.reverse
				.dropWhile{ case (w,t) => t.parent.flag('nil)}
				.map{_._1}
				.toArray
		}
		//--Filter
		val good:Iterable[GoodOutput] = O.ckyCountType match {
			case O.CkyCountType.all => 
				//(all parses valid)
				val total:Double = correct.map{_.prob}.sum
				correct.map{ _.normalize(total,correct.length) }
			case O.CkyCountType.bestAll => 
				//(all best-scoring parses valid)
				val maxProb = correct.maxBy( _.prob ).prob
				val ok = correct.filter( _.prob == maxProb )
				val total:Double = ok.map{_.prob}.sum
				ok.map{ _.normalize(total,ok.length) }
			case O.CkyCountType.bestRandom => 
				//(single best scoring parse valid)
				val maxProb = correct.maxBy( _.prob ).prob
				val ok = correct.filter( _.prob == maxProb )
				if(ok.length == 0){
					Array[GoodOutput]()
				} else {
					Array[GoodOutput](ok(0).normalize(ok(0).prob,1))
				}
			case O.CkyCountType.offsetZero => {
				//((has an offset zero term?))
				val hasZeroOffset:Boolean = correct.exists{ _.offset == 0L }
				//((get matching trees))
				val matching:Array[GoodOutput] = correct.filter{
						(output:GoodOutput) =>
					(!hasZeroOffset || output.offset == 0L)
				}
				//((create list))
				val total:Double = matching.map{_.prob}.sum
				matching.map{ _.normalize(total,matching.length) }
			}
			case O.CkyCountType.shortWithOffsetZero => {
				//((has an offset zero term?))
				val hasZeroOffset:Boolean = correct.exists{ _.offset == 0L }
				//((get shortest length))
				val shortest:Int = correct.foldLeft(Int.MaxValue){
						case (shortest:Int,output:GoodOutput) =>
					if(hasZeroOffset && output.offset == 0L){
						math.min(shortest, trim(output.tree,output.sent).length)
					} else {
						shortest
					}
				}
				//((get matching trees))
				val matching:Array[GoodOutput] = correct.filter{
						(output:GoodOutput) =>
					trim(output.tree,output.sent).length <= shortest &&
						(!hasZeroOffset || output.offset == 0L)
				}
				//((create list))
				val total:Double = matching.map{_.prob}.sum
				matching.map{ _.normalize(total,matching.length) }
			}
			case O.CkyCountType.mostNilsWithOffsetZero => {
				//((has an offset zero term?))
				val hasZeroOffset:Boolean = correct.exists{ _.offset == 0L }
				//((get shortest length))
				val shortest:(Int,Int) = correct
						.foldLeft( (Int.MaxValue,Int.MaxValue) ){
						case ((shortestTrim:Int,nonNils:Int),output:GoodOutput) =>
					if(hasZeroOffset && output.offset == 0L){
						val (trim,nonNilCount) = countNonNils(output.tree,output.sent)
						if(trim == shortestTrim){
							(trim,math.min(nonNils,nonNilCount))
						} else if(trim < shortestTrim){
							(trim,nonNilCount)
						} else {
							(shortestTrim,nonNils)
						}
					} else {
						(shortestTrim,nonNils)
					}
				}
				//((get matching trees))
				val (shortestTrim,nonNils) = shortest
				val matching:Array[GoodOutput] = correct.filter{
						(output:GoodOutput) =>
					val (trim,nonNilCount) = countNonNils(output.tree,output.sent)
					trim <= shortestTrim && nonNilCount <= nonNils &&
						(!hasZeroOffset || output.offset == 0)
				}
				//((create list))
				val total:Double = matching.map{_.prob}.sum
				matching.map{ _.normalize(total,matching.length) }
			}
			case _ => throw fail("Unknown case: " + O.ckyCountType)
		}
		//--Return
		//(debug)
		assert(correct.length == 0 || good.exists( _.prob > 0.0 ), 
			"All outputs have zero mass" )
		//(return)
		good
	}

	def handleParses(
			parses:Array[EvalTree[Any]],
			gold:Temporal,
			grounding:Time,
			score:Score,
			sent:TimeSent,
			allParsesAreFree:Boolean=false
			):Iterable[GoodOutput] = {
		//--Util
		val viterbi:Option[Temporal] = 
			if(parses != null && parses.length > 0) {
				val parse:Any = parses(0).evaluate
				parse match {
					case (time:Temporal) => Some(time(grounding))
					case _ => throw new IllegalStateException("Not a temporal: " + parse)
				}
			} else {
				None
			}
		//--Score Parses
		val scoresUnsorted:Array[ScoreElem]
			//(for each parse...)
			= parses.zipWithIndex.foldLeft((List[ScoreElem](),false)){ 
					case ((soFar:List[ScoreElem],isPruned:Boolean),
					      (parse:EvalTree[Any],i:Int)) => 
				if(!isPruned){
					//(variables)
					val ground:GroundedRange 
						= if(O.guessRange){ grounding.guessRange }
						  else{ Range(grounding,grounding) }
					val parseProb = parse.logProb
					//(timing)
					val parseWatch:Stopwatch = new Stopwatch
					parseWatch.start
					//(debug variables)
					var lastProb = Double.PositiveInfinity
					//(get guess temporal)
					val guess:Temporal = parse.evaluate match {
						case (t:Temporal) => t
						case _ => throw new IllegalStateException("Not a temporal: "+parse)
					}
					//(for each offset of parse...)
					val compareRes = compare(guess,gold,ground).slice(0,O.scoreBeam).toList
					val totalProb:Double = compareRes.foldLeft(0.0){ _ + _.prob }
					val rtn = soFar ::: compareRes.map{ (elem:CompareElem) =>
							val prob = elem.prob / totalProb
							//(debug)
//							assert(prob <= lastProb,  //TODO enable me
//								"Times are not monotonically decreasing: "+lastProb+"->"+
//									prob+" from " + guess)
							lastProb = prob
							//(create parse)
							val parseProb = if(allParsesAreFree){ 0.0 } else { parse.logProb }
							val resultLogProb = 
								if(O.includeTimeProb){ parseProb+math.log(prob) }
								else{ parseProb }
							ScoreElem(i,elem.offset,elem.diff,resultLogProb,guess)
						}
					//(timing & return)
					val lapTime = parseWatch.lap
					if(lapTime > O.pruneTime && i > O.pruneMinIndex){
						log(FORCE,"pruning after " + i)
						(rtn,true)
					} else {
						(rtn,false)
					}
				} else {
					(soFar,isPruned)
				}
		}._1.toArray
		val scores = 
			if(O.sortTimeProbInScore){
				scoresUnsorted.sortWith{ case (a:ScoreElem,b:ScoreElem) => 
					//(case: order by P(parse)*P(time))
					if( (b.logProb - a.logProb).abs < 1e-6 ){
						if(a.index == b.index){
							b.offset.abs > a.offset.abs
						} else {
							b.index > a.index
						}
					} else {
						b.logProb < a.logProb 
					}
				}
			} else {
				scoresUnsorted
			}
		log("" + scores.length + " candidates")
		//--Score Parses
		score.lock.acquire
		val isCorrect:Boolean = if(scores.length == 0){
			//(case: no scores)
			score.enter(false,(Duration.INFINITE,Duration.INFINITE), -1)
			score.store(sent,viterbi.orNull,gold,false,grounding)
			score.logFailure(sent,gold,grounding)
			false
		} else {
			//(case: have score)
			//((get guess))
			val bestGuess = scores(0)
//			assert(O.timeDistribution != O.Distribution.Point ||  //TODO enable me
//				bestGuess.offset == 0,
//				"Sanity check for time distribution")
			//((is in beam?))
			val correct:Array[ScoreElem] = scores.filter{ (elem:ScoreElem) => 
				assert(!elem.logProb.isNaN && elem.logProb <= 0.0, 
					"invalid probability")
				elem.exact }
			//((enter score))
			if(bestGuess.exact){
				log(GREEN,"CORRECT")
			} else {
				log(FORCE,"best guess: " + bestGuess)
				if(bestGuess.offset != 0){
					log(FORCE,"then: " + scores(1))
				}
        if (scores.length > 1) {
          log(FORCE, "second best guess: " + scores(1))
        }
				log(FORCE,RED,"WRONG")
			}
			score.enter(bestGuess.exact,bestGuess.diff, 
				if(correct.length > 0) correct(0).index else -1)
			score.enterK(scores.slice(0,O.reportK).map{ _.exact })
			score.store(sent,viterbi.get,gold,bestGuess.exact,grounding)
			if(correct.length == 0){
				log(FORCE,RED,"" + correct.length + " in beam")
			} else {
				log("" + correct.length + " in beam")
			}
			bestGuess.exact
		}
		//--Log
		log({if(isCorrect) FORCE else null },"Guess:  " + viterbi.orNull)
		log({if(isCorrect) FORCE else null },"Gold:   " + gold)
		log({if(isCorrect) FORCE else null },"Tree:   " + {
			if(parses.length > 0){
				parses(0).asParseString(index.w2str(_),grammar.r2str(_)) 
			} else{ "" } })
		log({if(isCorrect) FORCE else null },"Ground: " + grounding)
		score.lock.release
		//--Post-Filter
		def allOutput:Iterable[GoodOutput] = scores.map{ (elem:ScoreElem) =>
			if(elem.exact){
				Some(GoodOutput(
					parses(elem.index),
					elem.temporal,
					elem.offset,
					math.exp(elem.logProb),
					grounding,
					sent))
			} else {
				None
			}
		}.filter{ _.isDefined }.map{ _.get }
		val filtered = filterCorrect(allOutput.toArray)
		//--Return
		//(debug)
		if(isCorrect){
			startTrack("Good Output")
		} else {
			forceTrack("Good Output")
		}
		filtered.foreach{ (t:GoodOutput) => 
			log(FORCE,"["+t.offset+"] "+
				"("+G.df.format(t.prob)+")"+
				t.tree.asParseString(index.w2str(_),grammar.r2str(_))) 
		}
		endTrack("Good Output")
		//(return)
		filtered
	}

	override def run:Unit = {
		//--Run
		forceTrack("Running")
		//(train)
		startTrack("Training")
		forceTrack("Creating Parser")
		val initialParser = CKYParser(
			index.W,
			grammar.rules.map{ (_,1.0) },
			grammar.factory,
			grammar.lexPrior,
			grammar.rulePrior )
		Execution.touchAndWrite("params-0", 
					initialParser.parameters(index.w2str(_),grammar.r2str(_)) )
		endTrack("Creating Parser")
		log("Threading on " + Execution.numThreads + " threads")
		val (parser,trainScoresRev):(CKYParser,List[Score]) 
			= if(O.runInterpretModel){
				(0 until O.iters).foldLeft( (initialParser,List[Score]()) ){
						case ((parser:CKYParser,scores:List[Score]),iter:Int) =>
				forceTrack("Iteration " + iter)
				//(create score)
				val score = new Score
				var goodParses:List[GoodOutput] = List[GoodOutput]()
				val parseLock = new scala.concurrent.Lock()
				//(create tasks)
				val tasks:java.lang.Iterable[Runnable] = asJavaIterable(
					data.train.eachExample(iter).zipWithIndex.map{
							case ((sent:TimeSent,gold:Temporal,ground:Time),i:Int) =>
					new Runnable{ override def run {
						startTrack("[timex " + i + "] " + sent.toString)
						//((parse))
						val parses:Array[EvalTree[Any]] = parser.parse(sent,O.beam)
						//((handle parses))
						val filteredParses = handleParses(parses, gold, ground, score, sent, 
							iter == 0 && O.uniformFirstIteration)
						//((store parses))
						endTrack("[timex " + i + "] " + sent.toString)
						parseLock.acquire
						goodParses = goodParses ::: filteredParses.toList
						parseLock.release
					}}
				})
				//(run tasks)
				threadAndRun("Parsing", tasks, Execution.numThreads)
				log(FORCE,"finished parsing")
				//(debug)
				assert(goodParses.toArray.length == 0 || 
					goodParses.exists( _.prob > 0.0 ), 
					"All good parses have zero mass" )
				log("finished parsing")
				//(filter prob>0 parses)
				val nonZeroGoodParses = goodParses
						.filter{ _.logProb > Double.NegativeInfinity }
				//(update parser)
				val newParser = parser.update(
					nonZeroGoodParses
						.map{ (o:GoodOutput) => 
							new ReweightedParseTree(o.tree,o.logProb) 
						},
					grammar.lexPrior,
					grammar.rulePrior
					)
				log("updated parser")
				//(update time)
				startTrack("Updating Times")
				var toUpdate = List[Temporal]()
				//((E-step))
				nonZeroGoodParses.foreach{ 
						case GoodOutput(tree,value,offset,prob,ground,s) =>
					val gr:GroundedRange = Range(ground,ground)
					assert(prob > 0.0 && prob <= 1.0, "Bad time probability: " + prob)
//					forceTrack(tree.asParseString(index.w2str(_),grammar.r2str(_)))
					value.traverse(gr,offset,
						(term:Temporal,trueOffset:Long) => {
//							log(FORCE,"Touching " + term + " " + term.getClass)
							toUpdate = term :: toUpdate
							term.updateE(gr,trueOffset,if(O.hardEM) 0 else U.safeLn(prob))
						}
					)
//					endTrack(tree.asParseString(index.w2str(_),grammar.r2str(_)))
				}
				//((M-step))
				nonZeroGoodParses.foreach{ 
						case GoodOutput(tree,value,offset,prob,ground,s) =>
					val gr:GroundedRange = Range(ground,ground)
					value.traverse(gr,offset,
						(term:Temporal,trueOffset:Long) => term.runM)
				}
				toUpdate.foreach{ _.runM }
				endTrack("Updating Times")
				//(debug)
				startTrack("Parameters")
				Execution.touchAndWrite("params-"+(iter+1), 
					newParser.parameters(index.w2str(_),grammar.r2str(_)) )
				endTrack("Parameters")
				//(continue loop)
				log(FORCE,BOLD,YELLOW,""+score)
				log(FORCE,YELLOW,""+score.reportK)
				endTrack("Iteration " + iter)
				(newParser,score :: scores)
			}
		} else {
			val parser:CKYParser = 
				try {
					log("Loading parser at: " + O.interpretModel)
					IOUtils.readObjectFromFile(O.interpretModel)
					.asInstanceOf[TreeTime]
					.parser
				} catch {
					case (e:Throwable) => throw new RuntimeException(e)
				}
			val trainScoresRev:List[Score] = List[Score]()
			(parser,trainScoresRev)
		}
		endTrack("Training")
		//(test)
		startTrack("Eval")
		val testScore = new Score
		data.eval.eachExample(Int.MaxValue).zipWithIndex.foreach {
					case ((sent:TimeSent,gold:Temporal,ground:Time),i:Int) =>
			startTrack("[" + i + "] "+{if(O.devTest){ sent.toString } else { "" }})
			//((parse))
			val parses:Array[EvalTree[Any]] = parser.parse(sent,O.beam)
			//((handle parses))
			handleParses(parses, gold, ground, testScore, sent)
			//((continue map))
			endTrack("[" + i + "] " + {if(O.devTest){ sent.toString } else { "" }})
		}
		log(FORCE,BOLD,YELLOW,""+testScore)
		log(FORCE,YELLOW,""+testScore.reportK)
		endTrack("Eval")
		endTrack("Running")
		//--Save Model
		val sys = new TreeTime(parser,grammar,lex)
		try {
			import org.goobs.util.TrackedObjectOutputStream
			import java.io.FileOutputStream
			IOUtils.writeObjectToFile(sys,Execution.touch("interpretModel.ser.gz"))
		} catch {
			case (e:Throwable) => throw new RuntimeException(e)
		}
		//--Score
		startTrack(FORCE,BOLD,"Results")
		try {
			reportScores(sys,trainScoresRev.reverse.toArray,testScore)
		} catch {
			case (e:Exception) => log(FORCE,e)
		}
		endTrack("Results")
	}

	def reportScores(sys:TreeTime,trainScores:Array[Score],testScore:Score) {
		val logger = Execution.getLogger();
		//--External Score
		if(O.train.source == O.DataSource.English) {
			startTrack("TempEval")
			//(run)
			startTrack("Train")
			val (trnOfficial,trnAngel) 
				= Entry.officialEval(sys,data.train.asInstanceOf[Iterable[Annotation]],
				true,O.tempevalHome,false,Execution.touch("attr-train.tab"))
			endTrack("Train")
			startTrack("Eval")
			val (tstOfficial,tstAngel) 
				= Entry.officialEval(sys,data.eval.asInstanceOf[Iterable[Annotation]],
				false,O.tempevalHome,false,Execution.touch("attr-test.tab"))
			endTrack("Eval")
			//(print)
			startTrack("Eval Results")
			log(FORCE,BOLD,GREEN,"TreeTime Train:         " + trnOfficial)
			logger.setGlobalResult("interpret.train.tempeval.type", trnOfficial.typeAccuracy)
			logger.setGlobalResult("interpret.train.tempeval.value", trnOfficial.valueAccuracy)
			log(FORCE,BOLD,GREEN,"TreeTime Train (Angel): " + trnAngel)
			log(FORCE,BOLD,GREEN,"TreeTime Eval:         " + tstOfficial)
			logger.setGlobalResult("interpret.eval.tempeval.type", tstOfficial.typeAccuracy)
			logger.setGlobalResult("interpret.eval.tempeval.value", tstOfficial.valueAccuracy)
			log(FORCE,BOLD,GREEN,"TreeTime Eval (Angel): " + tstAngel)
			endTrack("Eval Results")
			endTrack("TempEval")
		}
		//--Process
		//(train)
		if(trainScores.length > 0){
			startTrack(BOLD,"train")
			logger.setGlobalResult("interpret.train.accuracy",
				trainScores(trainScores.length-1).accuracy)
			logger.setGlobalResult("interpret.train.averank",
				trainScores(trainScores.length-1).avePos)
			logger.setGlobalResult("interpret.train.inbeam",
				trainScores(trainScores.length-1).percentParsable)
			logger.setGlobalResult("interpret.train.score",
				trainScores(trainScores.length-1).aveScore())
			log(FORCE,BOLD,YELLOW,"train.accuracy: " + 
				trainScores(trainScores.length-1).accuracy)
			log(FORCE,YELLOW,"train.averank: " +	
				trainScores(trainScores.length-1).avePos)
			log(FORCE,YELLOW,"train.inbeam: " + 
				trainScores(trainScores.length-1).percentParsable)
			log(FORCE,YELLOW,"train.score: " + 
				trainScores(trainScores.length-1).aveScore())
			endTrack("train")
		}
		//(test)
		val s = if(O.devTest) "dev" else "test"
		startTrack(BOLD,s)
		logger.setGlobalResult("interpret."+s+".accuracy", testScore.accuracy)
		logger.setGlobalResult("interpret."+s+".averank", testScore.avePos)
		logger.setGlobalResult("interpret."+s+".inbeam", testScore.percentParsable)
		logger.setGlobalResult("interpret."+s+".score", testScore.aveScore())
		log(FORCE,BOLD,YELLOW,s+".accuracy: "+ testScore.accuracy)
		log(FORCE,YELLOW,s+".averank: "+ testScore.avePos)
		log(FORCE,YELLOW,s+".inbeam: "+ testScore.percentParsable)
		log(FORCE,YELLOW,s+".score: "+ testScore.aveScore())
		endTrack(s)
		//--Debug dump
		log("saving parses")
		trainScores.zipWithIndex.foreach( pair => {
			val (score,i) = pair
			val writer = new java.io.FileWriter(Execution.touch("train"+i))
			score.results.foreach( r => {
				writer.write(r.toString); writer.write("\n")
			})
			writer.close
		})
		val writer = new java.io.FileWriter(Execution.touch(s))
		testScore.results.foreach( r => {
			writer.write(r.toString); writer.write("\n")
		})
		writer.close
	}
}

//------------------------------------------------------------------------------
// AUXILLIARY CLASSES
//------------------------------------------------------------------------------
case class UNK()

object Score {
	def duration2double(d:Duration)
		= d.seconds.asInstanceOf[Double] / O.scoreGranularity.asInstanceOf[Double]
	def score(
			diff:(Double,Double), 
			c_over:Double = O.c_overconstraining,
			c_vague:Double = O.c_vagueness ):Double = {
		val (start,end) = diff
		val cStart:Double = if(start < 0){ c_over } else { c_vague }
		val cEnd:Double = if(end < 0){ c_vague } else { c_over }
		val scoreStart:Double = (cStart / (cStart + start.abs))
		val scoreEnd:Double = (cEnd / (cEnd + end.abs))
		assert(scoreStart >= 0 && scoreStart <= 1.0, "Start score must be in range")
		assert(scoreEnd >= 0 && scoreEnd <= 1.0, "End score must be in range")
		if(scoreStart < 1e-10 && scoreEnd < 1e-10) {
			0.0 //score of zero
		} else {
			(scoreStart+scoreEnd)/2.0
		}
	}
	def score(diff:(Duration,Duration)):Double = {
		score( (duration2double(diff._1),duration2double(diff._2)) )
	}
}

@SerialVersionUID(1L)
class Score {
	case class Result(sent:TimeSent,guess:Temporal,gold:Temporal,exact:Boolean,
			ground:Time) {
		override def toString:String = {
			"" + {if(exact) "HIT  " else if(guess != null) "MISS " else "FAIL "} +
			sent + " as " + 
			{if(guess != null) guess else "<fail>"} + 
			" <gold " + gold + ">"
		}
	}
	private var exactRight:Int = 0
	private var exactRightK:Array[Int] = new Array[Int](O.reportK)
	private var total:Int = 0
	private var sumPos = 0
	private var totalWithPos = 0
	private var totalWithNonzeroPos = 0
	private var goldMinusGuess = List[(Double,Double)]()
	private var resultList:List[Result] = List[Result]()
	private var failedList = List[(TimeSent,Temporal,Time)]()
	val lock = new scala.concurrent.Lock

	def releaseResults:Unit = { resultList = List[Result]() }
	def enter(exact:Boolean,diff:(Duration,Duration), position:Int) = {
		//(score exact)
		if(exact){ exactRight += 1 }
		total += 1
		//(store differences)
		goldMinusGuess
			= (Score.duration2double(diff._1), Score.duration2double(diff._2)) ::
				goldMinusGuess
		//(score position)
		if(position > 0){
			sumPos += position
			totalWithNonzeroPos += 1
		}
		if(position >= 0){
			totalWithPos += 1
		}
	}
	def enterK(topK:Array[Boolean]) = {
		assert(topK.length <= O.reportK, "Entering too many parses")
		(0 until exactRightK.length).foreach{ (i:Int) =>
			val anyOK:Boolean = !topK.slice(0,i+1).forall( !_ )
			if(anyOK){ exactRightK(i) += 1 }
		}
	}
	def store(sent:TimeSent,guess:Temporal,gold:Temporal,exact:Boolean,
			ground:Time)={
		resultList = Result(sent,guess,gold,exact,ground) :: resultList
	}
	def logFailure(sent:TimeSent,gold:Temporal,ground:Time) = {
		failedList = (sent,gold,ground) :: failedList
	}

	def accuracy:Double 
		= exactRight.asInstanceOf[Double]/total.asInstanceOf[Double]
	def avePos:Double 
		= sumPos.asInstanceOf[Double] / totalWithNonzeroPos.asInstanceOf[Double]
	def percentParsable:Double
		= (totalWithPos.asInstanceOf[Double] / total.asInstanceOf[Double])
	def aveScore(
			overconstrainingPenalty:Double = O.c_overconstraining, 
			vaguenessPenalty:Double = O.c_vagueness) = {
		val sum = goldMinusGuess.foldLeft(0.0)( 
			(soFar:Double,diff:(Double,Double)) => {
				val score = Score.score(diff,overconstrainingPenalty,vaguenessPenalty)
				assert(score == 0.0 || soFar+score > soFar, "Double overflow")
				soFar + score
			})
		sum / goldMinusGuess.length.asInstanceOf[Double]
	}
	def results:Array[Result] = resultList.reverse.toArray

	def reportK:String = {
		"beam quality: " + exactRightK.map{ (count:Int) =>
			G.df.format(count.asInstanceOf[Double] / total.asInstanceOf[Double]) 
		}.mkString("  ")
	}
	def reportFailures(y:(String=>Any)):Unit = {
		failedList.foreach{ case (sent:TimeSent,gold:Temporal,ground:Time) =>
			y(sent.toString + " :: " + gold + " :: " + ground)
		}
	}

	override def toString:String = {
		"accuracy: "+G.df.format(accuracy)+"; average pos: "+G.df.format(avePos)+
			" (in " + G.pf.format((percentParsable*100)) + "%); score: "+
			G.df.format(aveScore())
	}
}
//------------------------------------------------------------------------------
// TOY DATA(S)
//------------------------------------------------------------------------------
object ToyData {
	val lex = new Lex
	val index = Indexing()
	import lex._

	private val toys = new HashMap[String,Int]

	private val NONE = ToyStore(Array[(String,Temporal)](),false)
	private def store(test:Boolean,args:(String,Temporal)*):ToyStore 
		= ToyStore(args.toArray,test)
	//--Toy
	private val thisMorning = ("this morning",(TOD(1)))
	private val today = ("today",(TODAY))
	private val day = ("day",(DAY(todaysDate)))
	private val week = ("week",(WEEK(todaysDate)))
	private val aWeek = ("a week",(AWEEK))
	private val theWeek = ("the week",(WEEK(todaysDate)))
	private val thisWeek = ("this week",(REF ! AWEEK))
	private val lastWeekToday = ("last week today",(WEEK move -1))
	private val lastWeekNow = ("last week now",(WEEK move -1))
	private val lastWeek = ("last week",(WEEK move -1))
	private val aWeekAgo = ("a week ago",(Range(todaysDate,todaysDate) << AWEEK))
	private val weekLast = ("week last",(WEEK move -1))
	private val pastWeek = ("past week",(REF <| AWEEK))
	private val thePastWeek = ("the past week",(REF <| AWEEK))
	private val pastMonths2 = ("past 2 months",(REF <| (AMONTH*2)))
	private val pastYear = ("past year",(REF <| AYEAR))
	private val weeks2 = ("2 weeks",(AWEEK*2))
	private val months2 = ("2 months",(AMONTH*2))
	private val monthsdash2 = ("2 - month",(AMONTH*2))
	private val years2 = ("2 years",(AYEAR*2))
	private val year5 = ("5 years",(AYEAR*5))
	private val year5AndJunk = ("5 years in 2",(AYEAR*5))
	private val weeksDash2 = ("2 - weeks",(AWEEK*2))
	private val week2Period = ("2 week period",(AWEEK*2))
	private val month = ("month",(MONTH))
	private val aMonth = ("a month",(AMONTH))
	private val theMonth = ("the month",(MONTH))
	private val lastMonth = ("last month",(MONTH move -1))
	private val nextMonth = ("next month",(MONTH move 1))
	private val thisMonth = ("this month",(MONTH))
	private val spring = ("spring",(SEASON(1)))
	private val summer = ("summer",(SEASON(2)))
	private val fall = ("fall",(SEASON(3)))
	private val winter = ("winter",(SEASON(4)))
	private val quarter = ("quarter",(QUARTER))
	private val aQuarter = ("a quarter",(AQUARTER))
	private val lastQuarter = ("last quarter",(QUARTER move -1))
	private val firstQuarter =  ("1st quarter",(QOY(1)))
	private val secondQuarter = ("2st quarter",(QOY(2)))
	private val thirdQuarter = ( "3st quarter",(QOY(3)))
	private val fourthQuarter = ("4st quarter",(QOY(4)))
	private val y1776 = ("1776",(THEYEAR(1776)))
	private val y17sp76 = ("17 76",(THEYEAR(1776)))
	private val april = ("april",(MOY(4)))
	private val april1776 = ("april 1776",(MOY(4) ^ THEYEAR(1776)))
	private val april2 = ("april 2",(MOY(4) ^ DOM(2)))
	private val jan15 = ("january 15",(MOY(1) ^ DOM(15)))
	private val feb15 = ("february 15",(MOY(2) ^ DOM(15)))
	private val mar15 = ("march 15",(MOY(3) ^ DOM(15)))
	private val apr15 = ("april 15",(MOY(4) ^ DOM(15)))
	private val may15 = ("may 15",(MOY(5) ^ DOM(15)))
	private val jun15 = ("june 15",(MOY(6) ^ DOM(15)))
	private val jul15 = ("july 15",(MOY(7) ^ DOM(15)))
	private val aug15 = ("august 15",(MOY(8) ^ DOM(15)))
	private val sep15 = ("september 15",(MOY(9) ^ DOM(15)))
	private val oct15 = ("october 15",(MOY(10) ^ DOM(15)))
	private val nov15 = ("november 15",(MOY(11) ^ DOM(15)))
	private val dec15 = ("december 15",(MOY(12) ^ DOM(15)))
	private val year = ("year",(YEAR))
	private val ayear = ("a year",(AYEAR))
	private val lastYear = ("last year",(YEAR move -1))
	private val thisYear = ("this year",(YEAR))
	private val monday = ("monday",(DOW(1)(todaysDate,0)))
	private val tuesday = ("tuesday",(DOW(2)(todaysDate,0)))
	private val wednesday = ("wednesday",(DOW(3)(todaysDate,0)))
	private val thursday = ("thursday",(DOW(4)(todaysDate,0)))
	private val friday = ("friday",(DOW(5)(todaysDate,0)))
	private val saturday = ("saturday",(DOW(6)(todaysDate,0)))
	private val sunday = ("sunday",(DOW(7)(todaysDate,0)))
	private val monday_neg1 = ("monday",(DOW(1)(todaysDate,-1)))
	private val tuesday_neg1 = ("tuesday",(DOW(2)(todaysDate,-1)))
	private val wednesday_neg1 = ("wednesday",(DOW(3)(todaysDate,-1)))
	private val thursday_neg1 = ("thursday",(DOW(4)(todaysDate,-1)))
	private val friday_neg1 = ("friday",(DOW(5)(todaysDate,-1)))
	private val saturday_neg1 = ("saturday",(DOW(6)(todaysDate,-1)))
	private val sunday_neg1 = ("sunday",(DOW(7)(todaysDate,-1)))
	private val special_chars = ("today '",(REF(todaysDate)))
	private val lasthalf1989 = ("last half 1989",(Range(Time(1989,7),Time(1990))))
	private val lastquarter1989 = ("last quarter 1989",(Range(Time(1989,10),Time(1990))))
	private val recentMonths = ("recent months",(PAST))
	//--Hard Real Data
	private val may22sp1995 
		= ("May 22 , 1995", (Range(Time(1995,5,22),Time(1995,5,23))))


	private case class ToyStore(gold:Array[(String,Temporal)],test:Boolean) 
			extends DataStore[(TimeSent,Temporal,Time)] {
		override def name:String = if(test) "toy-dev" else "toy"
		override def eachExample(iter:Int):Iterable[(TimeSent,Temporal,Time)] = {
			val score:Score = new Score
			gold.zipWithIndex.map{ case ((sent:String,gold:Temporal),id:Int) =>
				//(variables)
				val words = sent.split(" ").map{ (raw:String) => 
					val (str,typ) = 
						if(raw.length > 2 && raw.endsWith("st") &&
								(raw.substring(0,raw.length-2) matches G.CanInt)){
							(raw.substring(0,raw.length-2),NumberType.ORDINAL)
						} else if(raw matches G.CanInt){
							(raw,NumberType.NUMBER)
						} else {
							(raw, NumberType.NONE)
						}
					(index.str2wTest(str),typ,str)
				}
				val s = TimeSent(
					words.map{ _._1 }, 
					words.map{ x => index.str2posTest("UNK") },
					words.map{ case (w:Int,t:NumberType.Value,str:String) =>
						if(t != NumberType.NONE){ str.toInt } else { Int.MinValue }
					},
					words.map{ _._2 },
					index
					)
				(s,gold(todaysDate,0),todaysDate)
			}
		}
		def internWords:ToyStore = {
			gold.foreach{ case (sent:String,gold:Temporal) =>
				sent.split(" ").foreach{ (str:String) => 
					index.str2w(str)
				}
			}
			this
		}
	}

	def TODAY_ONLY:TimeData[(TimeSent,Temporal,Time)] = {
		TimeData(store(false,today).internWords,store(true,today))
	}
	
	def STANDARD:TimeData[(TimeSent,Temporal,Time)] = {
		TimeData(
			store(false,
			//--Train
				//(durations)
				aWeek,aMonth,aQuarter,ayear,weeks2,months2,weeksDash2,week2Period,year5,
					year5AndJunk,
				//(sequences)
				week,month,quarter,year,day,theWeek,
				//(cannonicals -> sequences)
				thisWeek,thisYear,thisMonth,
				//(shifts -- standard)
				lastWeek,lastYear,lastQuarter,nextMonth,weekLast,aWeekAgo,
				//(shifts -- noncannonical)
				pastWeek,thePastWeek,pastYear,pastMonths2,
				//(numbers -- basic)
				y1776,
				//(sequences)
				april,
				//(intersects)
				jan15,feb15,mar15,apr15,may15,jun15,jul15,aug15,sep15,oct15,nov15,dec15,
				april1776,april2,
				//(days of the week)
				monday,tuesday,wednesday,thursday,friday,saturday,sunday,
				//(numbers -- complex)
				y17sp76,
				//(seasons)
				spring,summer,fall,winter,
				//(floor/ceil)
				quarter, firstQuarter, secondQuarter, thirdQuarter,fourthQuarter,

//				//(offset -1)
//				monday_neg1,tuesday_neg1,wednesday_neg1,thursday_neg1,friday_neg1,saturday_neg1,sunday_neg1,
//				//(hard)
//				lasthalf1989, lastquarter1989,recentMonths,
//				may22sp1995,special_chars,
				//(ref)
				today
			).internWords,
			//--Test
			store(true,lastMonth))
	}
}


