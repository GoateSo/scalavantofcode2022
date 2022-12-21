package solutions
import utils.Utils.*
import scala.collection.mutable.{ArraySeq, ListBuffer, Stack, HashMap}
import java.math.MathContext
import scala.math.BigDecimal.RoundingMode
class Day21(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample) {

  val map = Map(
    "*" -> ((x: BigDecimal, y: BigDecimal) => x * y),
    "+" -> ((x: BigDecimal, y: BigDecimal) => x + y),
    "-" -> ((x: BigDecimal, y: BigDecimal) => x - y),
    "/" -> ((x: BigDecimal, y: BigDecimal) => x / y)
  )

  enum Node:
    case Num(x: BigDecimal)
    case Op(l: String, r: String, var op: String)
  import Node.*

  val is = input
    .map(x =>
      // val r =
      x.find("(\\w+): (.+)".r) match
        case Seq(x, y) =>
          val ops = y.split(" ").toSeq
          val res =
            if ops.size == 1 then Num(ops(0).toInt)
            else Op(ops(0), ops(2), ops(1))
          (x, res)
    )
    .toMap

  def eval(n: String = "root"): BigDecimal =
    is(n) match
      case Num(x)       => x
      case Op(l, r, op) => map.apply(op)(eval(l), eval(r))

  override def run: Any = eval()

  // do some basic simplification to find whether humn is reached
  def const(x: String): Boolean =
    if x == "humn" then false
    else
      is(x) match
        case Num(_) => true
        case Op(l, r, op) =>
          const(l) && const(r)
  def evaluate(s: String): (BigDecimal, BigDecimal) =
    if s == "humn" then (1, 0)
    else
      is(s) match
        case Num(x) => (0, x)
        case Op(l, r, op) =>
          val (a, b) = evaluate(l)
          val (c, d) = evaluate(r)
          op match
            case "+" => (a + c, b + d)
            case "-" => (a - c, b - d)
            case "*" =>
              if a == 0 then (c * b, d * b)
              else if c == 0 then (a * d, b * d)
              else throw Exception("shouldn't happen")
            case "/" =>
              if c == 0 then (a / d, b / d)
              else throw Exception(s"${a}x+${b} , ${c}x+$d, $op ")
  override def run2: Any =
    val (l, r) = is("root") match
      case Num(x)       => ??? // shouldn't be the case
      case Op(l, r, op) => (l, r)
    val (res, quest) = if const(l) then (l, r) else (r, l)
    val (_, x) = evaluate(res)
    val (a, b) = evaluate(quest)
    ((x - b) / a).setScale(0, RoundingMode.HALF_UP)
}
