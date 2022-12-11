package solutions
import utils.Utils.*
import scala.collection.mutable.{ListBuffer, ArraySeq}
// import scala.collection.SeqFactory.Delegate.apply

class Day11(input : Seq[String]) extends Solution(input):
  case class Monkey(start : Seq[Long], op : Long => Long, m : Long, t : Int, f : Int) {}
  val blks = input.map(_.strip).splitBy(_.isEmpty)
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
      case n => (x : Long) => op(n.toLong, x)
    Monkey(
      starts,
      truOp, 
      monk(3).find("\\d+".r).toLong, 
      monk(4).find("\\d+".r).toInt, 
      monk(5).find("\\d+".r).toInt
    )

  def simulate(n : Int, fn : Long => Long): Long = 
    var (lvls, res) = (0 until n).foldLeft(
      IndexedSeq.fill(mks.size)(0l),
      mks.map(_._1).toIndexedSeq
    ){
      (acc, l) => 
        (0 until mks.size).foldLeft(acc){
          case ((nlvls, nvals), i) => 
            val Monkey(_, op, m, t, f) = mks(i)
            val vs = nvals(i)
            val (tvs, fvs) = vs.map(fn compose op).partition(_ % m == 0)
            val nvs = nvals.updated(i, List.empty)
              .updated (t, nvals(t) ++ tvs)
              .updated(f, nvals(f) ++ fvs)
            (nlvls.updated(i, nlvls(i) + vs.size), nvs)
        }
    }
    lvls.sorted.reverse.take(2).product
  override def run: Any = 
    simulate(20, _ / 3)

  override def run2: Any = 
    simulate(10000, _ % mks.map(_.m).product)

  // original sol:
  // def simulate(n : Int, fn : Long => Long): Long = 
  //   var values = scala.collection.mutable.ArraySeq.from(
  //     mks.map(xs => ListBuffer.from(xs.start.map(identity)))
  //   )
  //   val levels = Array.fill(values.size)(0l)
  //   for a <- 0 until n do
  //     for i <- 0 until values.size do
  //       val vs = values(i)
  //       val Monkey(_, op, m, t, f) = mks(i)
  //       levels(i) += vs.size
  //       for v <- vs do 
  //         val nv = fn(op(v))
  //         values(if nv % m == 0 then t else f) += nv
  //       values(i).clear()
  //   levels.sorted.reverse.take(2).product