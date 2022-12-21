package solutions

import utils.Utils.*
import scala.collection.mutable.{ArraySeq, ListBuffer, Stack, HashMap}
class Day20(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample) {

  val xs = input.map(_.toInt)
  def mix(xs: Seq[Int]): Seq[Int] =
    xs.foldLeft(xs)((acc, elem) =>
      // move element forward/backward by value of elem in cyclic list
      val ind = acc.indexOf(elem)
      val newInd = Math.floorMod(ind + elem, acc.size)

      // val newAcc = acc.updated(ind, 0)
      // newAcc.updated(newInd, elem)
      ???
    )
  override def run: Any =
    val res = mix(xs)
    val ind = res.indexOf(0)
    println(xs.count(_ == 0))
    ()

  override def run2: Any =
    ()
}
