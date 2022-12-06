package solutions
import utils.Utils.*

class Day6(input : Seq[String]) extends Solution(input):
  val xs = input(0).toList

  def first(len : Int) = 
    xs.sliding(len).indexWhere(x => x.distinct == x) + len

  // first occurence of 4 unique chars
  override def run = first(4)
  // first occurence of 14 unique chars
  override def run2 = first(14)