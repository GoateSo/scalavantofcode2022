package solutions

import utils.Utils.*
import scala.collection.mutable.{ListBuffer}
import scala.util.control.NonLocalReturns.*

class Day15(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):
  val is = input.toIndexedSeq
  var pts = is.map(s =>
    s.find(
      "^Sensor at x=(.+?), y=(.+?): closest beacon is at x=(.+?), y=(.+?)$".r
    ).map(_.toInt)
  )

  def getIntervals(bounds: Seq[(Int, Int)]) =
    var ints = ListBuffer[(Int, Int)]()
    val bds = bounds.sortBy(_._1)
    var curl = bds(0)._1
    var curr = bds(0)._2
    for i <- 1 until bounds.size do
      // write(curl,curr)
      val (l, r) = bds(i)
      if l > curr then
        ints += ((curl, curr))
        curl = l
        curr = r
      else curr = Math.max(curr, r)
    ints += ((curl, curr))
    ints

  def rowIntervals(row: Int) =
    val bounds = pts.flatMap { case Seq(x1, y1, x2, y2) =>
      val cd = Math.abs(x1 - x2) + Math.abs(y1 - y2)
      val dx = cd - Math.abs(y1 - row)
      if dx >= 0 then List((x1 - dx, x1 + dx)) else Nil
    }
    getIntervals(bounds)
  override def run: Any =
    val row = if isSample then 10 else 2000000
    val ints = rowIntervals(row)
    val beacons = pts.map(_.drop(2)).toSet
    val tot = ints.foldLeft(0) { case (acc, (l, r)) =>
      acc + (r - l + 1)
    }
    tot - beacons.count(_(1) == row)
  override def run2: Any =
    val dim = if isSample then 20 else 4000000
    returning {
      for row <- 0 until dim do
        val ints = rowIntervals(row)
        if ints.size > 1 then
          val (x, y) = (ints(0)._2 + 1, row)
          throwReturn(BigInt(x) * 4000000 + y)
    }
