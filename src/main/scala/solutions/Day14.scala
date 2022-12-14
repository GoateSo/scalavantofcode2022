package solutions
import utils.Utils.*
import scala.collection.mutable.{ListBuffer}
import scala.util.control.NonLocalReturns.*

class Day14(input : Seq[String]) extends Solution(input) {
  val paths = input.map( s =>
    s.split(" -> ").map(s2 => 
      val v2 = s2.strip().split(",")
      (v2(0).toInt, v2(1).toInt)
    )
  )

  var arr = Array.ofDim[Int](1000, 1000)

  for xs <- paths do
    var cur = xs.head
    for node <- xs.tail do
      val (x,y) = cur
      val (x2,y2) = node
      val dx = x2 - x
      val dy = y2 - y
      if dx == 0 then
        for $y <- y to y2 by dy.sign do
          arr(x)($y) = 1
      else
        for $x <- x to x2 by dx.sign do
          arr($x)(y) = 1
      cur = node

  val start = (500,0)

  def advance(p : (Int, Int)): (Int, Int) = p match
    case (x, y) if arr(x)(y + 1) == 0 => (x, y + 1)
    case (x, y) if arr(x - 1)(y + 1) == 0 => (x - 1, y + 1)
    case (x, y) if arr(x + 1)(y + 1) == 0 => (x + 1, y + 1)
    case _ => p

  val max = paths.flatten.map(_._2).max
  override def run: Any = 
    var n = 0
    returning {
      while true do
        var p = start
        while advance(p) != p do
          p = advance(p)
          if p._2 > paths.flatten.map(_._2).max then
            throwReturn(n)
        arr(p._1)(p._2) = 2
        n+=1
    }
    n

  override def run2: Any = 
    // clear of 2s
    arr = arr.map(_.map{
      case 2 => 0
      case x => x
    })

    val max = paths.flatten.map(_._2).max + 2
    for i <- 0 until 1000 do
      arr(i)(max) = 1

    var n = 0
    while arr(start._1)(start._2) == 0 do
      var p = start
      while advance(p) != p do
        p = advance(p)
      arr(p._1)(p._2) = 2
      n+=1
    n
}
