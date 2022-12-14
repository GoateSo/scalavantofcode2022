package utils
import scala.util._
import scala.math._
import scala.util.matching._
import scala.util.matching.Regex.Match
import scala.collection.mutable.{ArrayBuffer, HashMap}
import os._
import scala.compiletime.ops.string

/** collection of useful utility methods & shorthands
  */
object Utils:
  // alphanbetic regex shorthand
  val al = "[a-zA-Z]".r

  // modulo using the sign of the divisor
  extension (x: Int) def +%(y: Int) = Math.floorMod(x, y)
  extension (x: Long) def +%(y: Long): Int = Math.floorMod(x, y).toInt

  extension [T, U](p: (T, U))
    def bimap(f: T => U, g: U => T) = (f(p._1), g(p._2))

  // exponentiation
  extension (n: Double) def **(m: Double) = Math.pow(n, m)

  // random string stuff kinda like in lua's string library
  extension (str: String)
    def sub(start: Int, end: Int) = str.substring(start, end)
    // convert str -> int or str -> Long using given radix
    def toLong = java.lang.Long.parseLong(str)
    def toLong(radix: Int) = java.lang.Long.parseLong(str, radix)
    def toInt = Integer.parseInt(str)
    def toInt(radix: Int) = Integer.parseInt(str, radix)

    def find(reg: Regex): Seq[String] =
      reg.findFirstMatchIn(str).map(_.subgroups).getOrElse(Seq())
    def findOrElse(reg: Regex, back: String): String =
      reg.findFirstIn(str).getOrElse(back)
    def gsub(reg: Regex, f: Seq[String] => String) =
      reg.replaceAllIn(
        str,
        _ match {
          case reg(xs*) => f(xs)
        }
      )
    def gsub(reg: Regex, f: Seq[String] => String, times: Int) =
      var occurs = 0
      reg.replaceSomeIn(
        str,
        _ match {
          case reg(xs*) if occurs < times =>
            occurs += 1
            Some(f(xs))
          case _ => None
        }
      )
    def gsub(reg: Regex, rep: String) =
      reg.replaceAllIn(str, rep)
    def gsub(reg: Regex, rep: String, times: Int) =
      var occurs = 0
      reg.replaceSomeIn(
        str,
        _ match {
          case reg(_*) if occurs < times =>
            occurs += 1
            Some(rep)
          case _ => None
        }
      )

  extension [T](strs: Seq[T])
    // split by predicate or string
    def splitBy(p: T => Boolean) = strs.foldLeft(Seq(Seq.empty[T])) {
      case (acc, s) if p(s) => acc :+ Seq.empty[T]
      case (acc, s)         => acc.init :+ (acc.last :+ s)
    }
    def splitBy(v: T): Seq[Seq[T]] = splitBy(_ == v)
    // distinct, but not an iterator (so i don't forget the name)
    def unique = strs.distinct.toSeq

  extension [T](grid: Seq[Seq[T]]) def columns = grid.transpose

  extension (lines: Seq[String]) def chrCols = lines.map(_.toSeq).transpose

  // print to folder; reduce clutter
  inline def write(xs: Any*) =
    os.write.append(pwd / "POutput.txt", (xs mkString " ") + "\n")

  def euclid(x: Int, y: Int): Int =
    if y == 0
    then x
    else euclid(y, x % y)

  extension (xs: List[Int])
    def gcd = xs.reduce(euclid)
    def median = xs.sorted.apply(xs.size / 2)
    def mean = xs.sum / xs.size.toDouble

  // shorthands
  extension (i: Int)
    def toBin = i.toBinaryString
    def toHex = i.toHexString

  /** inclusive random integer
    */
  def randInt(b1In: Int, b2In: Int) =
    val (mi, ma) = if b1In > b2In then (b2In, b1In) else (b1In, b2In)
    Random.between(mi, ma + 1)

  // priority queue w/ deckey operation
  class MinPq[T](xs: T, priority: Double):
    var arr = ArrayBuffer(null, (xs, priority))
    val map = HashMap(xs -> 1)

    override def toString =
      arr.mkString("[", ",", "]") ++ "\n" ++ map.mkString("[", ",", "]")

    def +=(n: T, priority: Double): Unit =
      arr += ((n, priority))
      map(n) = arr.length - 1
      swim(arr.length - 1)

    def top = arr(1)

    def pop: (T, Double) =
      val ret = arr(1)
      swap(1, arr.length - 1)
      map -= ret._1
      arr = arr.take(arr.length - 1)
      sink(1)
      ret

    def decKey(n: T, nDist: Double) =
      val i = map(n)
      arr(i) = (n, nDist)
      swim(i)

    def swap(i: Int, j: Int): Unit =
      map(arr(i)._1) = j
      map(arr(j)._1) = i
      val n = arr(i)
      arr(i) = arr(j)
      arr(j) = n

    def swim(i: Int): Unit =
      var p = i
      while p > 1 && arr(p / 2)._2 > arr(p)._2 do
        swap(p / 2, p)
        p /= 2

    def sink(i: Int): Unit =
      val l = i * 2
      val r = l + 1
      var smol = i
      if l < arr.length && arr(l)._2 < arr(smol)._2 then smol = l
      if r < arr.length && arr(r)._2 < arr(smol)._2 then smol = r
      if smol != i then
        swap(i, smol)
        sink(smol)
  // digraph with weights
  class Digraph[T] {
    var adj = HashMap[T, Set[(T, Double)]]()
    def addEdge(from: T, to: T, weight: Double = 1) =
      adj(from) = adj.getOrElse(from, Set.empty[(T, Double)]) + ((to, weight))
    def addEdges(from: T, tos: Seq[(T, Double)]) =
      adj(from) = adj.getOrElse(from, Set.empty[(T, Double)]) ++ tos
    def bfs(start: T, f: T => Unit): Unit =
      import scala.collection.mutable.{Set, Queue}
      var visited = Set(start)
      var queue = Queue(start)
      while queue.nonEmpty do
        val n = queue.dequeue()
        f(n)
        for (e, w) <- adj(n) do
          if !visited.contains(e) then
            visited += e
            queue.enqueue(e)

    def dfs(start: T, f: T => Unit): Unit =
      import scala.collection.mutable.{Set, Stack}
      var visited = Set(start)
      var stack = Stack(start)
      while stack.nonEmpty do
        val n = stack.pop()
        f(n)
        for (e, w) <- adj(n) do
          if !visited.contains(e) then
            visited += e
            stack.push(e)
    def path(start: T, end: T): (Double, List[T]) =
      // perform dijkstra's algorithm on graph using minPQ from above
      val dist = HashMap[T, Double]()
      val prevs = HashMap[T, T]()

      def getPath(x: T): List[T] =
        if x == start then List(start)
        else x :: getPath(prevs(x))

      val pq = MinPq[T](start, 0)
      dist(start) = 0
      var retval = (Double.MaxValue, List.empty[T])
      while pq.arr.length > 1 do
        val (n, nDist) = pq.pop
        if n == end then retval = (nDist, getPath(n))
        else
          for (e, w) <- adj(n) do
            val newDist = nDist + w
            if !dist.contains(e) || newDist < dist(e) then
              dist(e) = newDist
              prevs(e) = n
              pq += (e, newDist)
      retval
    override def toString(): String = adj.mkString("\n")

    // def pathNeg(start : T, end : T)
  }
  def toDigraph[T](arr: Array[Array[T]]): Digraph[T] =
    // constuct digraph between array elements and their 4 neighors in grid
    val g = Digraph[T]()
    for i <- 0 until arr.length do
      for j <- 0 until arr(0).length do
        val n = arr(i)(j)
        val neighs = Seq(
          if i > 0 then Some((arr(i - 1)(j), 1.0)) else None,
          if i < arr.length - 1 then Some((arr(i + 1)(j), 1.0)) else None,
          if j > 0 then Some((arr(i)(j - 1), 1.0)) else None,
          if j < arr(0).length - 1 then Some((arr(i)(j + 1), 1.0)) else None
        ).flatten
        g.addEdges(n, neighs)
    g
  def toWeightedDigraph(arr: Array[Array[Double]]): Digraph[(Int, Int)] =
    // constuct digraph between array elements and their 4 neighors in grid
    val g = Digraph[(Int, Int)]()
    for i <- 0 until arr.length do
      for j <- 0 until arr(0).length do
        val n = (i, j)
        val neighs = Seq(
          if i > 0 then Some(((i - 1, j), arr(i - 1)(j))) else None,
          if i < arr.length - 1 then Some(((i + 1, j), arr(i + 1)(j)))
          else None,
          if j > 0 then Some(((i, j - 1), arr(i)(j - 1))) else None,
          if j < arr(0).length - 1 then Some(((i, j + 1), arr(i)(j + 1)))
          else None
        ).flatten
        g.addEdges(n, neighs)
    g

  def plot(pts: (Int, Int)*): Unit =
    val x1 = Math.min(pts.map(_._1).min, -1)
    val y1 = Math.min(pts.map(_._2).min, -1)
    val x2 = Math.max(pts.map(_._1).max, 6)
    val y2 = Math.max(pts.map(_._2).max, 2)

    val npts = pts.toSet
    var sb = new StringBuilder()
    for y <- y2 to y1 by -1 do
      for x <- x1 to x2 do
        if npts.contains((x, y)) then
          sb += '#' // pts.count(_ == (x, y)).toString
        else if x % 5 == 0 && y % 5 == 0 then sb += '+'
        else if x % 5 == 0 || x % 5 == 0 then sb += '|'
        else if y == 0 then sb += '-'
        else sb += '.'
      sb += '\n'
    // println(sb.toString)
    write(sb.toString)
    // val plt = new Plot()
    // plt.addLinePlot("plot", x.toArray, y.toArray)
    // plt.show()
