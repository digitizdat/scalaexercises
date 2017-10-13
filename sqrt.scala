package org.mcgreal.sqrt

import org.backuity.clist._
// or if you do not like wildcard imports:
// import org.backuity.clist.{Command, opt, args}

object Sqrt {
    class SqrtCli extends Command(description = "return the square root of the given number") {
        var number = arg[Double](description = "files to concat")
    }

    def getsqrt(number: Double) = {
        var threshold = 0.001
        
        def abs(x: Double) = if (x > 0) x else -x
        
        def mean(x: Double, y: Double) = (x+y)/2
        
        def guess(x: Double) = 1.0
        
        def isGoodEnough(cur: Double) =
            abs(cur-number) <= threshold
        
        def sqrtIter(guess: Double): Double =
            if (isGoodEnough(guess*guess)) guess
            else sqrtIter(mean(guess, number/guess))
        
        sqrtIter(guess(number))
    } 

    def main(argv: Array[String]) {
        Cli.parse(argv).withCommand(new SqrtCli) { case pargs =>
            println(getsqrt(pargs.number))
        }
        println(getsqrt(pargs.number))
    }
}
