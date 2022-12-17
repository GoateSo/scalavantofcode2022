package solutions
import utils.Utils.*

class Day16 (input : Seq[String], isSample : Boolean = false) extends Solution(input,isSample):
  val xs = input map { i =>
    val ms = i.find("Valve (..) has flow rate=(\\d+); (.+)".r)
    
    (ms(0), ms(1).toInt, ms(2).dropWhile(!_.isUpper).split(", ").toList)
  }

  write(xs mkString "\n")

  val graph = Digraph[String]()
  for (x, _, ys) <- xs  do
    for y <- ys do
      graph.addEdge(x, y)

  val rates = xs.map(x => (x._1, x._2)).toMap

  write(rates)

  write(graph)
  override def run: Any = 
    // maximize flow rate over time
    // find nodes with positive flow rate 
    val pfr = xs.filter(_._2 > 0)
    // start at valve AA
    val start = "AA"
    // find shortest paths from each node to every other node
    val paths = for 
      (x, _, _) <- pfr
      (y, _, _) <- pfr
      if x != y
    yield (x,y,graph.path(x, y)._1)
    write(paths.groupBy(_._1).mapValues { xs => 
        xs map ((a,b,c) => (b,c))
      }.mkString("\n")
    )
    val spaths = pfr.map((a,_,_) => graph.path(start, a))
    write(spaths)
    // completely search thru all combos of paths
    
  override def run2: Any = 
    
    ()
