package solutions
import utils.Utils.*

class Day10(input : Seq[String]) extends Solution(input) {
  // hacky; add noop to addx operation to make it 2 cycles
  val is = input flatMap ( v => 
    if v == "noop" then Seq(v) else Seq("noop", v)
  )
  // run the sequence of ops
  val ts = is.scanLeft(1) {
    case (a, b) => if b == "noop" then a else a + b.drop(5).toInt
  }

  override def run = 
    // 20, 60, 100, 140, 180, 220 = 40x + 20
    (20 to 220 by 40).map(x => x*ts(x-1)).sum

  override def run2 = 
    val sb = java.lang.StringBuilder()
    // CRT 40 X 6
    for i <- 0 until 240 do
      if (i%40 == 0) then
        sb.append("\n")
      sb.append(if Math.abs(ts(i)-i%40) <= 1 then "*" else".")
    sb.toString.trim
}
