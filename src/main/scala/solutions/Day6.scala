package solutions
import utils.Utils.*

class Day6(input : Seq[String]) extends Solution(input):
  val xs = input(0).toList

  def first(len : Int) = 
    val windows = xs.sliding(len).toList
    val fst = windows.indexOf(windows.find(x => x.distinct == x).get)
    fst + len

  // first occurence of 4 unique chars
  override def run(): Unit =
    println(first(4))
  // first occurence of 14 unique chars
  override def run2(): Unit = 
    println(first(14))
