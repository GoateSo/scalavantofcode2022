package solutions

import utils.Utils.*
import scala.collection.mutable.{ArraySeq, ListBuffer, Stack, HashMap}
class Day20(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample) {

  val xs = input.map(_.toLong)

  def mix(seq: Seq[(Long, Int)]): Seq[(Long, Int)] =
    val temp = seq
    temp
      .sortBy(_._2)
      .foldLeft(temp) { case (acc, elem) =>
        if elem._1 == 0 then acc
        else
          val ind = acc.indexOf(elem)
          val (a, _ +: rest) = acc.splitAt(ind): @unchecked
          val nseq = a ++ rest
          val e = elem._1
          val nind = ((ind + e - 1) +% (acc.size - 1)) + 1
          val (l, r) = (nseq.take(nind), nseq.drop(nind))
          val ret = l ++ (elem +: r)
          ret
      }

  override def run: Any =
    val result = mix(xs.zipWithIndex)
    val res = result.toIndexedSeq
    val i = res.indexWhere(_._1 == 0)
    res((i + 1000) % res.size)._1
      + res((i + 2000) % res.size)._1
      + res((i + 3000) % res.size)._1

  override def run2: Any =
    val num = 811589153L
    val nxs = xs.map(_ * num).zipWithIndex
    val res = (1 to 10)
      .foldLeft(nxs) { case (acc, n) =>
        mix(acc)
      }
      .map(_._1)
    val i = res.indexWhere(_ == 0)
    res((i + 1000) % res.size)
      + res((i + 2000) % res.size)
      + res((i + 3000) % res.size)

}
