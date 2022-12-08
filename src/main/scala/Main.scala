import java.io._
import scala.io._
import solutions._

import utils.Utils.*

@main def main: Unit =
  println("real")
  val input2 = Source.fromFile("input.txt").getLines.toList
  val sol2 = new Day8(input2)
  println(sol2.run)
  println(sol2.run2)

  println("samples:")
  val input = Source.fromFile("sample.txt").getLines.toList
  val sol = new Day8(input)
  println(sol.run)
  println(sol.run2)
  
