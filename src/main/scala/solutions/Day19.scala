package solutions

import utils.Utils.*
import scala.collection.mutable.{ArraySeq, ListBuffer, Stack, HashMap}

class Day19(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample) {
  val bps =
    input.map(x =>
      x.dropWhile(_ != ':')
        .substring(1)
        .split("\\. ")
        .toSeq
        .map(x =>
          x.find("Each (\\w+) robot costs ([^.]+)".r) match
            case Seq(x, y) => (x, y.split(" and ").toSeq)
        )
    )
  write(bps.map(_ mkString "\n") mkString "\n\n")

  override def run: Any =
    val robots = HashMap("ore" -> 1)
    val reses = for i <- 0 until bps.size yield
      val bp = bps(i)
      val robots = HashMap(
        "ore" -> 1,
        "clay" -> 0,
        "obsidian" -> 0,
        "geode" -> 0
      )
      val minerals = HashMap[String, Int](
        "ore" -> 0,
        "clay" -> 0,
        "obsidian" -> 0,
        "geode" -> 0
      )
      var max = 0
      // maximize the number of geode robots at each turn
      // G(n) = G(n-k?) + ???
      for _ <- 1 to 24 do ???
      ???
    // start with 1 ore robot
    ()

  override def run2: Any =
    ()
}
