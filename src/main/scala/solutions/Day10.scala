package solutions
import utils.Utils.*

class Day10(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):
  // hacky; add noop to addx operation to make it 2 cycles
  val is = input flatMap (v => if v == "noop" then Seq(v) else Seq("noop", v))
  // run the sequence of ops
  val ts = is.scanLeft(1) {
    case (a, b) if b == "noop" => a
    case (a, b)                => a + b.drop(5).toInt
  }

  override def run =
    // 20, 60, 100, 140, 180, 220 = 40x + 20
    (20 to 220 by 40).map(i => i * ts(i - 1)).sum

  override def run2 =
    // CRT 40 X 6
    (0 until 6).map { i =>
      (0 until 40).map { j =>
        val ind = i * 40 + j
        if Math.abs(ts(ind) - ind % 40) <= 1 then '#' else '.'
      }.mkString
    } mkString ("\n")

  // original imperative code:
  // val sb = new java.lang.StringBuilder()
  // for i <- 0 until 240 do
  //   if (i%40 == 0) then sb.append("\n")
  //   sb.append(if Math.abs(ts(i)-i%40) <= 1 then "#" else".")
  // sb.toString.trim
