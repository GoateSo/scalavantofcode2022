package solutions

import utils.Utils.*
import scala.collection.mutable.{HashSet, HashMap}


// tryna make this functional might be a nightmare
class Day17 (input : Seq[String], isSample : Boolean = false) extends Solution(input,isSample):
  // shapes as offsets
  val shapes = Array(
    List((0,0),(1,0),(2,0),(3,0)), //-
    List((1,0),(0,1),(1,1),(2,1),(1,2)),//+
    List((0,0),(1,0),(2,0),(2,1),(2,2)), // backwards L
    List((0,0),(0,1),(0,2),(0,3)), // |
    List((0,0),(1,0),(0,1),(1,1)) // square
  )

  // offsets for shifting
  val os = input(0).toSeq.map {
    case '<' => -1
    case '>' => 1
  }

  // integrate part 2 solution into both
  def simulate(runs : Long): Long = 
    // simulate tetraminos = 
    val ts = HashSet[(Int,Int)]()
    var i = 0
    var a = 0l
    // sum of cycles in simulation; separate from non-cyclic portion
    var cycleSum = 0l
    val seen = HashMap[HashSet[(Int,Int)], (Long, Long)]()
    while a < runs do
      var curs = shapes((a % shapes.length).toInt)
      val maxy = ts.maxByOption(_._2).map(_._2).getOrElse(0)
      var y = 4 + maxy
      var x = 2
      var canpush, canfall = true
      while 
        // push
        val nx = x + os(i % os.length)
        val nv = for (a,b) <- curs yield (a + nx, b + y)
        canpush = nv.forall((a,b) => !ts.contains(a,b) && a >= 0 && a <= 6) 
        if canpush then x = nx
        // drop
        val ny = y - 1
        val nv2 = for (a,b) <- curs yield (a + x, b + ny)
        canfall = nv2.forall((a,b) => !ts.contains(a,b) && b >= 1) 
        if canfall then y = ny
        // increment and check whether it's at rest
        i+=1
        canfall 
      do {}
      // add tetramino to set
      ts.addAll(curs.map((a,b) => (a + x, b + y)))
      a += 1
      val ymax = ts.maxBy(_._2)._2
      // caveman brain cycling
      val tops = ts.filter(_._2 >= ymax - 28).map((a,b) => (a, b - ymax + 28))
      if seen.contains(tops) then
        val (pa, pymax) = seen(tops)
        val da = a - pa           // difference in index
        val dymax = ymax - pymax // difference in height
        val reps = (runs - a) / da
        cycleSum += reps * dymax
        a += reps * da
      seen(tops) = (a, ymax)
    ts.map(_._2).max + cycleSum

  override def run: Any = 
    simulate(2022)

  override def run2: Any = 
    simulate(1000000000000l)


    // original tetramino simulation code (part 1)
    // val ts = HashSet[(Int,Int)]()
    // // simulate tetraminos
    // var i = 0
    // println(os.size)
    // for s <- 0 until 2022 do
    //   var curs = shapes(s % shapes.length)
    //   val maxy = ts.maxByOption(_._2).map(_._2).getOrElse(0)
    //   var y = 4 + maxy
    //   var x = 2
    //   var canpush, canfall = true
    //   while 
    //     // push
    //     val nx = x + os(i % os.length)
    //     val nv = curs.map((a,b) => (a + nx, b + y))
    //     canpush = nv.forall(!ts.contains(_))
    //       && nv.forall((a,b) => a >= 0 && a <= 6)
    //     if canpush then
    //       x = nx
    //     val ny = y - 1
    //     val nv2 = curs.map((a,b) => (a + x, b + ny))
    //     canfall = nv2.forall(!ts.contains(_)) && nv2.forall((a,b) => b >= 1)
    //     if canfall then
    //       y = ny
    //     i+=1
    //     canfall 
    //   do {}
    //   ts.addAll(curs.map((a,b) => (a + x, b + y)))
    // ts.map(_._2).max