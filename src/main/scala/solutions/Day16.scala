package solutions
import utils.Utils.*

class Day16 (input : Seq[String], isSample : Boolean = false) extends Solution(input,isSample):
  val xs = input map { i =>
    val ms = i.find("Valve (..) has flow rate=(\\d+); (.+)".r)
    (ms(0), ms(1).toInt, ms(2).dropWhile(!_.isUpper).split(", ").toList)
  }

  val graph = Digraph[String]()
  for (x, _, ys) <- xs  do
    for y <- ys do
      graph.addEdge(x, y)

  val rates = xs.map(x => (x._1, x._2)).toMap

  val pfr = xs.filter(_._2 > 0)
  val start = "AA"
  val paths = for 
    (x, _, _) <- pfr
    (y, _, _) <- pfr
    if x != y
  yield (x,y,graph.path(x, y)._1)
  val dists = paths.groupBy(_._1).mapValues(
    _.map((a,b,c) => (b,c)).sortBy(_._2)
  )
  val spaths = pfr.map((a,_,_) => (graph.path(start, a)._1, a))
  def simulate(cur : String, minutes : Int, on : Set[String], flow: Int = 0): Int = 
    // at valve, after movement, with the valve still closed (or everything opened)
    val rate = on.map(rates).sum
    if minutes < 0 then 
      Integer.MIN_VALUE
    else if minutes <= 1 then 
      flow + rate * minutes
    else
      val nmin =  minutes - 1
      val nflow = flow + rate
      val on2 = on + cur
      val nrate = rate + rates(cur)
      val adjs = dists(cur).filter { 
        case (s,l) => !on(s) && minutes > l+1
      }
      if adjs.isEmpty then
        nflow + nrate * nmin
      else
        val ls = for (neighbor, dist) <- adjs yield
          val tgain = nrate * dist.toInt
          val remTime = nmin - dist.toInt
          simulate( neighbor, remTime, on2, nflow + tgain)
        ls.max
  override def run: Any = 
    var maxFlow = 0
    val res = for (w, node) <- spaths yield
      simulate(node, 30 - w.toInt, Set.empty)
    res.max
  override def run2: Any = 
    
    ()
