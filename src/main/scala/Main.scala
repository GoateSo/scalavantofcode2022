import solutions.*
import scala.io.AnsiColor.*
import os.*
import utils.Utils

@main def main: Unit =
  val x = println(pwd)
  val realInputs = os.read.lines(pwd/"input.txt")
  val sampleInputs = os.read.lines(pwd/"sample.txt")
  def emph(s : String) = s"${BOLD}${UNDERLINED}${BLUE}$s${RESET}"
  def ans(s : String) = s"${RED}$s${RESET}"
  os.write.over(pwd/"POutput.txt", "")

  println(emph("[sample]"))
  val s2 = Day13(sampleInputs)
  println(s"${GREEN}part 1: ${RESET}")
  println(ans(s2.run.toString))
  Utils.write("+".repeat(120))
  println(s"${GREEN}part 2: ${RESET}")
  println(ans(s2.run2.toString))

  Utils.write("~".repeat(120))

  println(emph("[real]"))
  val s = Day13(realInputs)
  println(s"${GREEN}part 1: ${RESET}")
  println(ans(s.run.toString))
  Utils.write("+".repeat(120))
  println(s"${GREEN}part 2: ${RESET}")
  println(ans(s.run2.toString))