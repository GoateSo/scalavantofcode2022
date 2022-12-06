package solutions
import utils.Utils.*
class Day1(input : Seq[String]) extends Solution(input):
    // split by empty lines and map to sum of calories
    val elves = input.splitBy(_.isEmpty).map(_.map(_.toInt).sum)

    def run(): Unit = 
        // get maximum
        println(elves.max)

    def run2(): Unit =
        // get max 3
        println(elves.sorted.takeRight(3).sum)