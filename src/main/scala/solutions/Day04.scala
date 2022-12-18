package solutions
import utils.Utils.*

class Day04(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):
  // parse inputs
  val xs = input.map(_.split(",").map(_.split("-").map(_.toInt).toList).toList)
  override def run =
    xs count { // filter for those that are totally contained
      case List(List(a, b), List(x, y)) =>
        a <= x && y <= b || x <= a && b <= y
      case _ => false
    }
  override def run2 =
    xs.filterNot { // filter out those that are disjoint
      case List(List(a, b), List(x, y)) =>
        b < x || a > y
      case _ => true
    }.size
