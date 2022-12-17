package solutions
import utils.Utils.*
import scala.collection.mutable.{ListBuffer, ArraySeq, HashSet}

class Day12(input : Seq[String], isSample : Boolean = false) extends Solution(input,isSample):
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

  def bfs(start : (Int, Int)): Int = 
    var steps = 0
    // perform bfs on graph
    val visited = HashSet[(Int,Int)](start)
    var queue = List[(Int,Int)](start)
    var found = false
    while !queue.isEmpty do
      steps += 1
      found ||= queue.contains(end)
      queue = if found 
        then Nil 
        else queue.flatMap( (i,j) => 
          List((i+1,j),(i-1,j),(i,j+1),(i,j-1))
            .filter((ni,nj) => 
              val ret = 
                   ni >= 0 && ni < ys.size 
                && nj >= 0 && nj < ys(ni).size 
                && ys(ni)(nj)-ys(i)(j) <= 1 
                && !visited.contains((ni,nj))
              if ret then 
                visited += ((ni,nj))
              ret
            )
        )
    // check if smth was actually found instead of the subgraph being exhausted
    if found then steps else Int.MaxValue

  def simulate(starts : Seq[(Int, Int)]): Int = 
    starts.map(bfs).min - 1

  override def run = 
    simulate(Seq(strt))

  override def run2 = 
    simulate(for 
      i <- 0 until ys.size 
      j <- 0 until ys(i).size if ys(i)(j) == 'a' 
    yield (i,j))
