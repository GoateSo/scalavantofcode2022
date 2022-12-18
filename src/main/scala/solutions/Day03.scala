package solutions
import utils.Utils.*

class Day03(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):
  // priority function (a-z = 1-26, A-Z = 27-52)
  def pri(v: Char) =
    if v.isLower then v - 'a' + 1 else v - 'A' + 27

  val lines = input.toList.map(_.strip)

  override def run =
    lines
      .map(s =>
        val (a, b) = s.splitAt(s.length / 2) // split in half
        (a.toSet & b.toSet)
          .map(pri)
          .sum // find all common chars, and sum their priorities
      )
      .sum // sum all lines

  override def run2 =
    lines
      .sliding(3, 3)
      .map( // get in groups of 3
        _.map(_.toSet) // convert to sets
          .reduce(_ & _)
          .map(pri)
          .sum // find all common chars, and sum their priorities
      )
      .sum // sum all groups
