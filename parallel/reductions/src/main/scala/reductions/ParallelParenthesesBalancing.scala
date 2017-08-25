package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def loop(chars: Array[Char], b: Int): Int = {
      if (chars.isEmpty) b
      else {
        chars.head match {
          case '(' => loop(chars.tail, b + 1)
          case ')' => if (b > 0) loop(chars.tail, b - 1) else -1
          case _ => loop(chars.tail, b)
        }
      }
    }
    loop(chars, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, remainingOpen: Int, remainingClose: Int): (Int, Int) = {
      var from = idx
      var (toOpen, toClose) = (remainingOpen, remainingClose)
      while (from < until) {
        chars(from) match {
          case '(' => toClose += 1
          case ')' => toOpen -= 1
        }
        from += 1;
      }
      (toOpen, toClose)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (
          (toOpen1, toClose1),
          (toOpen2, toClose2)
        ) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        (toOpen1 + toClose2, toOpen2 + toClose1)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
