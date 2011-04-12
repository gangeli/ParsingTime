package time

//(lib)
import org.goobs.exec.Execution
import org.goobs.exec.Log._

object Entry {
	def main(args:Array[String]):Unit = {
		Execution.exec(new Runnable(){
			override def run:Unit = {
				log("Hello World!")
			}
		}, args)
	}
}
