package time

import org.goobs.database._

@Table(name="timebank_doc")
class TimebankDocument extends org.goobs.testing.Datum{
	@PrimaryKey(name="fid")
	private var fid:Int = 0
	@Key(name="filename")
	private var filename:String = null
	@Key(name="notes")
	private var notes:String = null
	@Child(localField="fid", childField="fid")
	var sentences:Array[TimebankSentence] = null
	@Child(localField="fid", childField="fid")
	var links:Array[TLink] = null
	override def getID = fid
}

@Table(name="timebank_sent")
class TimebankSentence extends DatabaseObject{
	@PrimaryKey(name="sid")
	private var sid:Int = 0
	@Key(name="fid")
	private var fid:Int = 0
	@Key(name="length")
	private var length:Int = 0
	@Key(name="gloss")
	private var gloss:String = null
//	@Child(localField="sid", childField="sid")
//	private var tags:Array[TimebankTag] = null
	@Child(localField="sid", childField="sid")
	var timexes:Array[Timex] = null
}

@Table(name="timebank_tag")
class TimebankTag extends DatabaseObject{
	@Key(name="wid")
	private var wid:Int = 0
	@Key(name="sid")
	private var sid:Int = 0
	@Key(name="did")
	private var did:Int = 0
	@Key(name="key")
	private var key:String = null
	@Key(name="value")
	private var value:String = null
}

@Table(name="timebank_timex")
class Timex extends DatabaseObject{
	@PrimaryKey(name="tid")
	private var tid:Int = 0
	@Key(name="sid")
	private var sid:Int = 0
	@Key(name="scopeBegin")
	private var scopeBegin:Int = 0
	@Key(name="scopeEnd")
	private var scopeEnd:Int = 0
	@Key(name="type")
	private var timeType:String = null
	@Key(name="value")
	private var value:Array[String] = null
	@Key(name="temporalFunction")
	private var temporalFunction:Boolean = false
	@Key(name="functionInDocument")
	private var functionInDocument:String = null
	@Key(name="gloss")
	private var gloss:String = null

	override def toString:String = {
		"" + tid + ": " + gloss
	}
	override def equals(other:Any):Boolean = {
		return other.isInstanceOf[Timex] && other.asInstanceOf[Timex].tid == tid
	}
	override def hashCode:Int = tid
}

@Table(name="timebank_tlink")
class TLink extends DatabaseObject{
	@PrimaryKey(name="lid")
	private var lid:Int = 0
	@Key(name="fid")
	private var fid:Int = 0
	@Key(name="source")
	private var sourceTimexId:Int = 0
	@Key(name="target")
	private var targetTimexId:Int = 0
	@Key(name="type")
	private var linkType:String = null
}



