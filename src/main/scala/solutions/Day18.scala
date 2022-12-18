package solutions
import utils.Utils.*
import scala.collection.mutable.Stack
import scala.compiletime.ops.int
class Day18(input : Seq[String], isSample : Boolean = false) extends Solution(input,isSample) {
  val poses = input.map(_.split(",").toSeq.map(_.toInt)).toSet

  write(poses mkString "\n")
  
  val mx = poses.map(_(0)).max
  val my = poses.map(_(1)).max
  val mz = poses.map(_(2)).max

  val mix = poses.map(_(0)).min
  val miy = poses.map(_(1)).min
  val miz = poses.map(_(2)).min

  def neighbors(x : Int, y : Int, z : Int) = 
    IndexedSeq(
      IndexedSeq(x+1,y,z),
      IndexedSeq(x-1,y,z),
      IndexedSeq(x,y+1,z),
      IndexedSeq(x,y-1,z),
      IndexedSeq(x,y,z+1),
      IndexedSeq(x,y,z-1)
    )

  override def run: Any = 
    // // val vsurf  = 
    // write(mx, my, mz)
    // write(mix, miy, miz)
    var n = 0
    for x <- mix  to mx do
      for y <- miy to my do
        for z <- miz to mz do
          if poses.contains(Seq(x,y,z)) then
            // check number of non- neighboring nodes, and add  
            n += neighbors(x,y,z).count(x => !poses.contains(x))
    n
  override def run2: Any = 
    var n = 0
    for x <- mix to mx do
      for y <- miy to my do
        for z <- miz to mz do
          if poses.contains(Seq(x,y,z)) then
            // check number of non- neighboring nodes, and add  
            n += neighbors(x,y,z).count(x => !poses.contains(x))
    n
    val grid = Array.fill(mx+1,my+1,mz+1)(0)
    
    

    for Seq(x,y,z) <- poses do
      grid(x)(y)(z) = 1
    // find number of islands inside the grid that are wholly contained in the grid
    // find connected components

    // keep track of each component as a set
    def dfs(x : Int, y : Int, z : Int) : Unit =
      var stack = Stack[(Int,Int,Int)]()
      stack.push((x,y,z))
      while !stack.isEmpty do
        val (x,y,z) = stack.pop()
        if grid(x)(y)(z) == 0 then
          write(x,y,z,"Aeiou")
          grid(x)(y)(z) = 2
          for Seq(a,b,c) <- neighbors(x,y,z) 
          do
            if a >= mix && a <= mx && b >= miy && b <= my && c >= miz && c <= mz &&
              grid(a)(b)(c) == 0 then
              stack.push((a,b,c))
    for x <- 0 to mx do
      for y <- 0 to my do
        for z <- 0 to mz do
          if x <= mix || y <= miy || z <= miz || x == mx || y == my || z == mz then
            if grid(x)(y)(z) == 0 then
              dfs(x,y,z)

    for x <- mix to mx do
      for y <- miy to my do
        for z <- miz to mz do
          if grid(x)(y)(z) == 0 then
            write(x,y,z,"aeiou")
            // check number of non- neighboring nodes, and add  
            n -= neighbors(x,y,z).count{ case Seq(x,y,z) => 
              x >= mix && x <= mx && y >= miy && y <= my && z >= miz && z <= mz &&
              grid(x)(y)(z) == 1 
            }
    n
}
