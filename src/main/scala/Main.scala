import solutions._
import os._

@main def main: Unit =
  val x = println(pwd)
  val realInputs = os.read.lines(pwd/"input.txt")
  val sampleInputs = os.read.lines(pwd/"sample.txt")

  os.write.over(pwd/"POutput.txt", "")
  
  println("real")
  val s = Day9(realInputs)
  println(s.run)
  println(s.run2)
  utils.Utils.write("~".repeat(120))
  println("sample")
  val s2 = Day9(sampleInputs)
  println(s2.run)
  println(s2.run2)