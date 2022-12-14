package solutions
import utils.Utils.*
import scala.collection.mutable.{ListBuffer}
import scala.compiletime.ops.boolean
class Day13(input : Seq[String]) extends Solution(input):
  // how many pairs in right order
  val pairs =  input.splitBy(_.isEmpty)

  def commaSplit(input : Seq[Char]): Seq[Seq[Char]] = 
    var left, right = 0
    var k = 1
    var i = 1
    var ret = ListBuffer[Seq[Char]]()
    for c <- input.drop(1) do
      i += 1
      c match
        case '[' => left += 1
        case ']' => right += 1
        case ',' => if left == right then 
          ret ++= Seq(input.slice(k, i-1))
          k = i
        case _ => ()
    ret ++= Seq(input.slice(k, i-1))
    ret.toList
  
  def parse(input : Seq[Char]): List[Any] = 
    // write("i",input)
    if input.mkString == "[]" then
      List()
    else if input.head != '[' then
      List(input.takeWhile(_.isDigit).mkString.toInt)
    else
      val xs = commaSplit(input)
      xs.map(parse).toList
  
  def isGood(xs : List[Any], ys : List[Any]): (Boolean, Boolean) = 
    val ret = (xs, ys) match
      case (Nil, Nil) => (true, false)
      case (Nil, _) => (true, true)
      case (_, Nil) => (false, true)
      case (x :: xs, y :: ys) =>
        val r = (x, y) match
          case (a : Int, b : Int) => 
            if a == b then isGood(xs, ys) else 
              (a < b, true)
          case (a : List[Any], b : List[Any]) => 
            val (i, e) = isGood(a, b)
            if e then (i, e) else 
              val (i2, e2) = isGood(xs, ys)
              (i && i2, e2)
          case (a : Int, b : List[Any]) => 
            val (i, e) = isGood(List(a), b)
            if e then (i, e) else 
              val (i2, e2) = isGood(xs, ys)
              (i && i2, e2)
          case (a : List[Any], b : Int) =>  
            val (i, e) = isGood(a, List(b))
            if e then (i, e) else 
              val (i2, e2) = isGood(xs, ys)
              (i && i2, e2)
          case _ => (false, true)
        r
    ret
  
  override def run: Any = 
    // write(parse("[1,2,3]"))
    val xs = pairs.map(
      ls => 
        val v = ls.map(s => parse(s.toSeq))
        (v.head, v.last)
    )
    val ys = xs.zipWithIndex.filter(x => 
      val v = isGood(x._1._1, x._1._2)
      v._1
    )
    ys.map(_._2+1).sum

  override def run2: Any = 
    val xs = List(List(2)) +: List(List(6)) +: pairs.map(
      ls => 
        ls.map(s => parse(s.toSeq))
    ).flatten
    // write( xs mkString "\n")
    
    val sorted = xs.sortWith(
      (a, b) => 
        val (i, e) = isGood(a, b)
        if e then i else 
          val (i2, e2) = isGood(b, a)
          !i2
    ) 
    val a = sorted.indexOf(List(List(2))) + 1
    val b = sorted.indexOf(List(List(6))) + 1
    write(a,b)
    a * b
