
import scala.util.Try

object Sqrt {
    def abs(x: Double) = if (x > 0) x else -x
        
    def getsqrt(number: Double) = {
        val threshold = 0.0001
        
        def mean(x: Double, y: Double) = (x+y)/2
        
        def guess(x: Double) = 1.0
        
        def isGoodEnough(cur: Double) =
            abs(cur-number) <= threshold
        
        def sqrtIter(guess: Double): Double =
            if (isGoodEnough(guess*guess)) guess
            else sqrtIter(mean(guess, number/guess))
        
        sqrtIter(guess(number))
    } 

    def output(result: Double, complex: Boolean) =
        if (complex == true) println(result + "i")
        else println(result)

    def main(argv: Array[String]) {
        val number = Try(argv(0).toDouble).getOrElse(0.0)

        if (number == 0.0) output(number, (number < 0))
        else output(getsqrt(abs(number)), (number < 0))
    }
}
