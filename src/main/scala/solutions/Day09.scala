package solutions
import utils.Utils.*

class Day09(input : Seq[String], isSample : Boolean = false) extends Solution(input,isSample):
  // direction mapping & input preprocessing
  val dirs = Map("R" -> (1, 0), "L" -> (-1, 0), "U" -> (0, 1), "D" -> (0, -1))
  val inputs = input.map(_.split(" ")).map(x => (dirs(x(0)), x(1).toInt))

  // check if p2 is in the 3x3 grid centered at p1
  def isAdj(p1 : (Int, Int), p2 : (Int, Int)): Boolean = 
    Math.abs(p1._1 - p2._1) <= 1 && Math.abs(p1._2 - p2._2) <= 1 

  // extremely inefficient, but it works
  // heavy use of eta expansion to make some things look better kekw
  def simulate(n : Int): Int = 
    // reduce using List of nodes and visited set as accumulator
    inputs.foldLeft(List.fill(n)(0,0), Set.empty) { 
      case ((nodes, vset), ((dx, dy), steps)) => 
        // scanLeft to get the list of nodes after each step
        val xs = (0 until steps).scanLeft(nodes) {
          case (ns, _) => 
            // build a new list of nodes from individual node movements, starting with the head
            ns.tail.foldLeft(List(ns.head.bimap(_ + dx, _ + dy))){ (acc, p) =>
              val (cx, cy) = acc.head
              val (tx, ty) = p
              val (dx, dy) = (cx-tx, cy-ty).bimap(_.sign, _.sign)
              // if the current and tail node aren't adjacent, move the tail node towards the current node
              val ret = if isAdj(acc.head, p) then p else (tx + dx, ty + dy)
              ret :: acc // add new node to accumulator
            }.reverse    // reverse to get the correct order (since I prepended, not appended)
        }
        xs.foreach(plot)
        // return the last node, add every concluding node to the visited set
        (xs.last, vset ++ xs.map(_.last))
    }._2.size // return the size of the visited set

  override def run: Any = 
    simulate(2)


  override def run2: Any = 
    simulate(10)

  /* 1st imperative version:
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
  */