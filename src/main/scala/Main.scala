import solutions.*
import scala.io.AnsiColor.*
import os.*
import utils.Utils

@main def main: Unit =
  val x = println(pwd)
  val realInputs = os.read.lines(pwd/"input.txt")
  val sampleInputs = os.read.lines(pwd/"sample.txt")
  def emph(s : String) = s"${BOLD}${WHITE_B}${BLUE}$s${RESET}"
  def ans(s : String) = s"${RED}$s${RESET}"
  os.write.over(pwd/"POutput.txt", "")
  
  println(emph("[real]"))
  val s = Day10(realInputs)
  println(ans(s.run.toString()))
  Utils.write("+".repeat(120))
  println(ans(s.run2.toString()))

  Utils.write("~".repeat(120))

  println(emph("[sample]"))
  val s2 = Day10(sampleInputs)
  println(ans(s2.run.toString()))
  Utils.write("+".repeat(120))
  println(ans(s2.run2.toString()))