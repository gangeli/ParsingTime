package time

class Time {
	override def toString:String = {
		"<time>"
	}
}

object Time {
	def apply(t:String) = {
		new Time
	}
}
