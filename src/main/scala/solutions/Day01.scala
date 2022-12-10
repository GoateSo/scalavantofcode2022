package solutions
import utils.Utils.*
class Day01(input : Seq[String]) extends Solution(input):
  // split by empty lines and map to sum of calories
  val elves = input.splitBy(_.isEmpty).map(_.map(_.toInt).sum)

  override def run = 
    // get maximum
    elves.max

  override def run2 =
    // get max 3
    elves.sorted.takeRight(3).sum
