import java.io._
import scala.io._
import solutions._

import utils.Utils.*

@main def main: Unit =
  println("samples:")
  val input = Source.fromFile("sample.txt").getLines.toList
  val sol = new Day6(input)
  sol.run()
  sol.run2()
  println("real")
  val input2 = Source.fromFile("input.txt").getLines.toList
  val sol2 = new Day6(input2)
  sol2.run()
  sol2.run2()

