package solutions

class Day8(input : Seq[String]) extends Solution(input):
  
  val ts = input.toIndexedSeq.map(_.toIndexedSeq)
  val xs = for i <- 1 until ts.size-1 yield
      for j <- 1 until ts(i).size-1 yield
        val x =  ts(i)(j)
        val u = (0 until i).map(ts(_)(j)).reverse
        val d = (i+1 until ts.size).map(ts(_)(j))
        val l = (0 until j).map(ts(i)(_)).reverse
        val r = (j+1 until ts(i).size).map(ts(i)(_))
        (x, List(u,d,l,r))
  override def run: Any = 
    xs.flatten.count {
      case (x, views) => 
        views.exists(_.max < x)
    } + 2*ts.size + 2*(ts(0).size-2)

  override def run2: Any = 
    xs.flatten.map {
      case (x, views) => 
        views.map(v => Math.min(v.size, v.takeWhile(_ < x).size+1)).product
    }.max
