package solutions
import utils.Utils.*
import scala.collection.mutable.Stack

class Day18(input : Seq[String], isSample : Boolean = false) extends Solution(input,isSample):
  val poses = input.map(_.split(",").toSeq.map(_.toInt)).toSet

  val mx = poses.map(_(0)).max
  val my = poses.map(_(1)).max
  val mz = poses.map(_(2)).max

  val mix = poses.map(_(0)).min
  val miy = poses.map(_(1)).min
  val miz = poses.map(_(2)).min

  def neighbors(x : Int, y : Int, z : Int) = 
    Seq(
      Seq(x+1,y,z),
      Seq(x-1,y,z),
      Seq(x,y+1,z),
      Seq(x,y-1,z),
      Seq(x,y,z+1),
      Seq(x,y,z-1)
    )

  var init = (for 
    x <- mix to mx
    y <- miy to my
    z <- miz to mz
    if poses.contains(Seq(x,y,z)) 
  yield neighbors(x,y,z).count(x => !poses.contains(x))).sum

  val grid = Array.fill(mx+1,my+1,mz+1)(0)
  
  for Seq(x,y,z) <- poses do
    grid(x)(y)(z) = 1

  def dfs(x : Int, y : Int, z : Int) : Unit =
    var stack = Stack[(Int,Int,Int)]()
    stack.push((x,y,z))
    while stack.nonEmpty do
      val (x,y,z) = stack.pop()
      if grid(x)(y)(z) == 0 then
        grid(x)(y)(z) = 2
        for Seq(a,b,c) <- neighbors(x,y,z) 
          if a >= mix && a <= mx && b >= miy && b <= my && c >= miz && c <= mz 
          if grid(a)(b)(c) == 0 
        do stack.push((a,b,c))
  override def run: Any = 
    init

  override def run2: Any = 
    var rem = 0

    for x <- mix to mx 
        y <- mix to my 
        z <- mix to mz 
        if x == mix || y == miy || z == miz || x == mx || y == my || z == mz
        if grid(x)(y)(z) == 0
    do dfs(x,y,z)

    val xs = for 
      x <- mix+1 to mx-1
      y <- miy+1 to my-1
      z <- miz+1 to mz-1 
      if grid(x)(y)(z) == 0
    yield neighbors(x,y,z).count{ case Seq(x,y,z) => grid(x)(y)(z) == 1}
    init - xs.sum
