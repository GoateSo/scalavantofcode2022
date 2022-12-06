package solutions
import utils.Utils.*

class Day3(inputs : Seq[String]) extends Solution(inputs):
  def pri(v : Char): Int = 
    if v.isLower then v - 'a' + 1 else v - 'A' + 27
  val lines = inputs.toList.map(_.strip)

override def run = 
  println(lines.map(s => 
    val (a,b) = s.splitAt(s.length / 2) // split in half
    (a.toSet & b.toSet).map(pri).sum    // find all common chars, and sum their priorities
  ).sum)                                  // sum all lines

override def run2 = 
  lines.sliding(3,3).map(                 // get in groups of 3        
    _.map(_.toSet)                      // convert to sets
    .reduce(_ & _).map(pri).sum         // find all common chars, and sum their priorities
  ).sum                                   // sum all groups
