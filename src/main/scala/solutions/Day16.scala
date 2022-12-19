package solutions
import utils.Utils.*

class Day16(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):
  // parse input
  val xs = input map { i =>
    val ms = i.find("Valve (..) has flow rate=(\\d+); (.+)".r)
    (ms(0), ms(1).toInt, ms(2).dropWhile(!_.isUpper).split(", ").toList)
  }

  // nodes with positive flow rate
  val pfr = xs.filter(_._2 > 0)
  val signods = pfr.map(_._1)
  // build graph from nodes, and compute shortest paths
  val graph = Digraph[String]()
  for (x, _, ys) <- xs do for y <- ys do graph.addEdge(x, y)
  // map of node: flow rate
  var rates = xs.map((a, b, c) => (a, b)).toMap
  // shortest paths between important nodes
  val paths = for
    x <- signods
    y <- signods
    if x != y
  yield (x, y, graph.path(x, y)._1)

  // important nodes (and their distances to other impotant nodes)
  var dists = paths
    .groupBy(_._1)
    .mapValues(
      _.map((a, b, c) => (b, c)).sortBy(_._2)
    )
    .toMap // add on starting node & its paths
    .updated("AA", signods.map(a => (a, graph.path("AA", a)._1)))

  def simulate(
      cur: String,
      minutes: Int,
      visited: Set[String],
      r: Int = 0 // rate
  ): Int =
    // at valve, after movement, with the valve still closed (or everything opened)
    // if time not sufficient to opne valve, conclude
    if minutes <= 1 then r * minutes
    else
      // calculate new time after opening (or not if flow rate is 0)
      val nmin = if rates(cur) != 0 then minutes - 1 else minutes
      // add current node to visited
      val on2 = visited + cur
      // calculate new rate from current node
      val nrate = r + rates(cur)
      // get adjacent nodes with
      //  - positive flow rate
      //  - can be reached and opened in time
      //  - not visited
      val adjs = dists(cur).filter { (s, l) =>
        !visited(s) && minutes > l + 1
      }
      // if no such nodes, conclude
      if adjs.isEmpty then r + nrate * nmin
      else
        // otherwise, simulate for each node, and get max, and factor in current rate
        r + (for (neighbor, dist) <- adjs yield
          // pressure released during transit
          val tgain = nrate * dist.toInt
          // remaining time after transit
          val remTime = nmin - dist.toInt
          tgain + simulate(neighbor, remTime, on2, nrate)
        ).max
  // most pressure released by me in 30 minutes
  override def run: Any =
    simulate("AA", 30, Set.empty)

  // most pressue released by me & elephant in 26 minutes
  override def run2: Any =
    val iseq = signods.toSet
    // partition visited, significant nodes to me & elephant, and get max
    // use bit representation of partition
    val res = for b <- 0 until (1 << signods.size) / 2 yield
      val (x, y) = iseq.zipWithIndex
        .partition { (s, i) =>
          (b & (1 << i)) != 0
        }
      simulate("AA", 26, x map (_._1))
        + simulate("AA", 26, y map (_._1))
    res.max
