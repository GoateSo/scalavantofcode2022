package solutions
import utils.Utils.*
import scala.collection.mutable.{ListBuffer, ArraySeq}

class Day11(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):
  // seperate into blocks for each monkey
  val blks = input.map(_.strip).splitBy(_.isEmpty)
  // create operation mapping
  val ops = Map[String, (Long, Long) => Long](
    "+" -> (_ + _),
    "*" -> (_ * _)
  )
  val mks = for monk <- blks yield
    val fn = monk(2).drop("Operation: new = old ".size).split(" ")
    val op = ops(fn(0))
    val truOp = fn(1) match
      case v if v == "old" => (x: Long) => op(x, x)
      case n               => (x: Long) => op(n.toLong, x)
    (
      // starting sequence
      monk(1)
        .drop("Starting items: ".size)
        .split(", ")
        .map(x => x.toLong)
        .toList,
      truOp, // operation
      monk(3).find("\\d+".r).head.toLong, // divisible by <m>
      monk(4).find("\\d+".r).head.toInt, // index if true
      monk(5).find("\\d+".r).head.toInt // index if false
    )

  def simulate(n: Int, fn: Long => Long): Long =
    var (lvls, res) =
      (1 to n).foldLeft( // each run, using levels & monkies as accumulator
        IndexedSeq.fill(mks.size)(0L),
        mks.map(_._1).toIndexedSeq
      ) { (acc, l) => // for each monkey, have them perform their throws
        (0 until mks.size).foldLeft(acc) { case ((nlvls, nvals), i) =>
          val (_, op, m, t, f) = mks(i)
          val vs = nvals(i)
          val (tvs, fvs) = vs.map(fn compose op).partition(_ % m == 0)
          val nvs = nvals
            .updated(i, List.empty) // clear the current list
            .updated(t, nvals(t) ++ tvs) // add the new values to true/false
            .updated(f, nvals(f) ++ fvs)
          (
            nlvls.updated(i, nlvls(i) + vs.size),
            nvs
          ) // update the levels and return both
        }
      }
    lvls.sorted.reverse
      .take(2)
      .product // return the product of the top 2 levels
  override def run: Any =
    simulate(20, _ / 3)

  override def run2: Any =
    simulate(10000, _ % mks.map(_._3).product)

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
