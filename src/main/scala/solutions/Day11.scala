package solutions
import utils.Utils.*
import scala.collection.mutable.{ListBuffer, ArraySeq, Buffer}
// import scala.collection.SeqFactory.Delegate.apply

class Day11(input : Seq[String]) extends Solution(input) {
  case class Monkey(
    start : ListBuffer[Long],
    op : Long => Long,
    m : Long,
    t : Int,
    f : Int
  ) {}
  val blks = input.splitBy(_.isEmpty).map(_.map(_.strip()))
  val ops = Map[String, (Long, Long) => Long](
    "+" -> (_ + _),
    "*" -> (_ * _),
  )
  val mks = for monk <- blks yield
    val starts = monk(1).drop("Starting items: ".size).split(", ").map(x => x.toLong).toSeq
    val fn = monk(2).drop("Operation: new = old ".size).split(" ")
    val op = ops(fn(0))
    val truOp = fn(1) match 
      case v if v == "old" => (x : Long) => op(x, x)
      case v => (x : Long) => op(fn(1).toLong, x)
    val m = (monk(3).drop("Test: divisible by ".size).toLong)
    val tcond = monk(4).drop("If true: throw to monkey ".size).toInt
    val fcond = monk(5).drop("If false: throw to monkey ".size).toInt
    Monkey(starts.to(ListBuffer), truOp, m, tcond, fcond)
    // (starts, truOp, m, tcond, fcond)

  val mod = mks.map(_.m).product

  def simulate(n : Int, fn : Long => Long): Long = 
    var values = ArraySeq.from(
      mks.map(_.start.map(identity))
    )
    val levels = Array.fill(values.size)(0l)
    for a <- 0 until n do
      for i <- 0 until values.size do
        val vs = values(i)
        val Monkey(_, op, m, t, f) = mks(i)
        levels(i) += vs.size
        for v <- vs do 
          val nv = fn(op(v))
          values(if nv % m == 0 then t else f) += nv
        values(i).clear()
    levels.sorted.reverse.take(2).product
  override def run: Any = 
    simulate(20, _ / 3)

  override def run2: Any = 
    simulate(10000, _ % mod)
}
