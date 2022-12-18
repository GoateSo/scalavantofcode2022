package solutions
import utils.Utils.*

class Day16(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):
  val xs = input map { i =>
    val ms = i.find("Valve (..) has flow rate=(\\d+); (.+)".r)
    (ms(0), ms(1).toInt, ms(2).dropWhile(!_.isUpper).split(", ").toList)
  }

  val signods = xs.filter(_._2 != 0).map(_._1)
  val graph = Digraph[String]()
  for (x, _, ys) <- xs do for y <- ys do graph.addEdge(x, y)

  var rates = xs.map(x => (x._1, x._2)).toMap

  val pfr = xs.filter(_._2 > 0)
  val paths = for
    (x, _, _) <- pfr
    (y, _, _) <- pfr
    if x != y
  yield (x, y, graph.path(x, y)._1)

  // starting paths
  val spaths = pfr.map((a, _, _) => (a, graph.path("AA", a)._1))
  var dists = paths
    .groupBy(_._1)
    .mapValues(
      _.map((a, b, c) => (b, c)).sortBy(_._2)
    )
    .toMap
    .updated("AA", spaths)

  def simulate(
      cur: String,
      minutes: Int,
      visited: Set[String],
      r: Int = 0 // rate
  ): Int =
    // at valve, after movement, with the valve still closed (or everything opened)
    // val rate = (if run2 then p2 else on).map(rates).sum
    // val rate = visited.map(rates).sum
    if minutes < 0 then Integer.MIN_VALUE
    else if minutes <= 1 then r * minutes
    else
      val nmin = if rates(cur) != 0 then minutes - 1 else minutes
      // val nflow = flow + rate
      val on2 = visited + cur
      val nrate = r + rates(cur)
      val adjs = dists(cur).filter { (s, l) =>
        !visited(s) && minutes > l + 1
      }
      if adjs.isEmpty then r + nrate * nmin
      else
        val ls = for (neighbor, dist) <- adjs yield
          val tgain = nrate * dist.toInt
          val remTime = nmin - dist.toInt
          tgain + simulate(neighbor, remTime, on2, nrate)
        ls.max + r
  override def run: Any =
    simulate("AA", 30, Set.empty)

  override def run2: Any =
    val iseq = signods.toIndexedSeq
    // partition visited, significant nodes to me & elephant, and get max
    val res = for b <- 0 until (1 << signods.size) / 2 yield
      val (x, y) = iseq.zipWithIndex.partition { (s, i) =>
        (b & (1 << i)) != 0
      }
      simulate("AA", 26, x.map(_._1).toSet)
        + simulate("AA", 26, y.map(_._1).toSet)

      // ()
    res.max
    //   for (node, w) <- spaths yield
    //     val ret = simulate(node, 30 - w.toInt, Set.empty)
    //     ret
    // res.max
    // ()
