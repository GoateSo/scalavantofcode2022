package solutions
import utils.Utils.*
import scala.collection.mutable.HashSet


class Day9(input : Seq[String]) extends Solution(input) {
  // val grid = input.toIndexedSeq.map(_.toIndexedSeq)
  write("a")
  
  val dmap = Map(
    "R" -> (1, 0),
    "L" -> (-1, 0),
    "U" -> (0, 1),
    "D" -> (0, -1)
  )
  val ps = input.map(_.split(" ")).map(x => (dmap(x(0)), x(1).toInt))
  write(ps.mkString("\n"))
  var vset = HashSet[(Int, Int)]((0,0)) // positions visited

  def isAdj(p1: (Int, Int), p2: (Int, Int)): Boolean = 
    // whether p1 is in 8 squres around p2
    val (x1, y1) = p1
    val (x2, y2) = p2
    val xdiff = Math.abs(x1-x2)
    val ydiff = Math.abs(y1-y2)
    xdiff <= 1 && ydiff <= 1

  def simulate(k : Int) = 
    var tposs = scala.collection.mutable.ArraySeq.fill(k-1)((0,0))
    var hpos = (0,0)
    for ((dx, dy), n) <- ps do
      for i <- 1 to n do
        hpos = (hpos._1 + dx, hpos._2 + dy)
        var cur = hpos
        for i <- 0 to k-2 do
          val (hx, hy) = cur
          val (tx, ty) = tposs(i)
          if !isAdj(cur, tposs(i)) then
            val xi = (hx-tx).sign
            val yi = (hy-ty).sign
            tposs(i) = (tx + xi, ty + yi)
            // vset += tposs(i)
          cur = tposs(i)
        vset+=tposs.last
    var ret = vset.size
    vset.clear()
    ret
  override def run = 
    simulate(2)

  override def run2 =
    simulate(10)

}
