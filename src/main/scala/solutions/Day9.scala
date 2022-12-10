package solutions
import scala.collection.mutable.{HashSet, ArraySeq}
import utils.Utils.*

class Day9(input : Seq[String]) extends Solution(input):
  val dirs = Map("R" -> (1, 0), "L" -> (-1, 0), "U" -> (0, 1), "D" -> (0, -1))
  val inputs = input.map(_.split(" ")).map(x => (dirs(x(0)), x(1).toInt))

  def isAdj(p1 : (Int, Int), p2 : (Int, Int)): Boolean = 
    Math.abs(p1._1 - p2._1) <= 1 && Math.abs(p1._2 - p2._2) <= 1

  // extremely inefficient, but it works
  // heavy use of eta expansion to make some things look better kekw
  def simulate(n : Int): Int = 
    inputs.foldLeft(List.fill(n)(0,0), Set.empty) { 
      case ((nodes, vset), ((dx, dy), steps)) => 
        val xs = (0 until steps).scanLeft(nodes) {
          case (ns, _) => 
            ns.tail.foldLeft(List(ns.head.bimap(_ + dx, _ + dy))){ (acc, p) =>
              val (cx, cy) = acc.head
              val (tx, ty) = p
              val (dx, dy) = (cx-tx, cy-ty).bimap(_.sign, _.sign)
              val ret = if isAdj(acc.head, p) then p else (tx + dx, ty + dy)
              ret :: acc
            }.reverse
        }
        (xs.last, vset ++ xs.map(_.last))
    }._2.size

  override def run: Any = 
    simulate(2)


  override def run2: Any = 
    simulate(10)
