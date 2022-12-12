package solutions
import utils.Utils.*
import scala.collection.mutable.{ListBuffer, ArraySeq, HashSet}

class Day12(input : Seq[String]) extends Solution(input) {
  val ys = input.toArray.map(_.toArray)
  var strt = (0,0)
  var end = (0,0)
  for i <- 0 until ys.size do
    for j <- 0 until ys(i).size do
      if ys(i)(j) == 'S' then
        strt = (i,j)
        ys(i)(j) = 'a'
      else if ys(i)(j) == 'E' then
        end = (i,j)
        ys(i)(j) = 'z'

  def bfs(starts : Seq[(Int, Int)]): Int = 
    var minSteps = Int.MaxValue
    for s <- starts do
      val visited = HashSet[(Int,Int)](s)
      var queue = ListBuffer[(Int,Int)]()
      queue += s
      var found = false
      var steps = 0
      while !queue.isEmpty do
        steps += 1
        val nq = ListBuffer[(Int,Int)]()
        for (i,j) <- queue do
          if ((i,j)) == end then
            found = true
            minSteps = Math.min(minSteps, steps-1)
          else
            for (di,dj) <- Seq((0,1),(0,-1),(1,0),(-1,0)) do
              val (ni,nj) = (i+di,j+dj)
              if ni >= 0 && ni < ys.size 
              && nj >= 0 && nj < ys(ni).size 
              && ys(ni)(nj)-ys(i)(j) <= 1 && !visited.contains((ni,nj)) then
                visited += ((ni,nj))
                nq += ((ni,nj))
        queue = nq
        if found then queue.clear()
    minSteps

  override def run = 
    bfs(Seq(strt))

  override def run2 = 
    bfs(for 
      i <- 0 until ys.size 
      j <- 0 until ys(i).size if ys(i)(j) == 'a' 
    yield (i,j))
  }
