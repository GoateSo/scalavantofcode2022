package solutions
import utils.Utils.*

class Day4(inputs : Seq[String]) extends Solution(inputs):
    val xs = inputs.map(_.split(",").map(_.split("-").map(_.toInt).toList).toList)
    override def run(): Unit = 
        println(
            xs.count({ // filter for those that are totally contained
                case List(List(a,b),List(x,y)) => 
                    a <= x && y <= b || x <= a && b <= y
                case _ => false
            })
        )

    override def run2(): Unit = 
        println(
            xs.filterNot({ // filter out those that are disjoint
                case List(List(a,b),List(x,y)) => 
                    b < x || a > y
                case _ => true
            }).size
        )