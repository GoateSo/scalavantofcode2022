package solutions
import utils.Utils.*

class Day06(input : Seq[String], isSample : Boolean = false) extends Solution(input,isSample):
  val xs = input(0).toList

  def first(len : Int) = 
    // get first occurence of a sequence of len chars that are all unique
    xs.sliding(len).indexWhere(x => x.distinct == x) + len

  override def run = first(4)
  
  override def run2 = first(14)
