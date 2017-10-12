
import org.backuity.clist._

object Sqrt extends CliMain[Unit](
    name = "sqrt",
    description = "return the square root of the given number") {
  
    var target = arg[Double](description = "files to concat")

    def sqrt() = {
        var threshold = 0.001
        
        def abs(x: Double) = if (x > 0) x else -x
        
        def mean(x: Double, y: Double) = (x+y)/2
        
        def guess(x: Double) = 1.0
        
        def isGoodEnough(cur: Double) =
            abs(cur-target) <= threshold
        
        def sqrtIter(guess: Double): Double =
            if (isGoodEnough(guess*guess)) guess
            else sqrtIter(mean(guess, target/guess))
        
        sqrtIter(guess(target))
    } 

    def run: Unit = {
        println("target = " + target)
    }
}
