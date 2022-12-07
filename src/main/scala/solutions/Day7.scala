package solutions
import utils.Utils.*
import scala.collection.mutable.{ArrayBuffer, HashMap}

class Day7(input : Seq[String]) extends Solution(input):
  // input : cmds
  var curdir = "./"
  var dirSizes = HashMap[String, List[Any]]()
  
  for line <- input.filterNot(_.startsWith("$ ls")) do
    if line.startsWith("$") then
      val ndir = line.drop(2).dropWhile(_ != ' ').strip
      curdir = ndir match
        case "/" => "./"
        case ".." => curdir.split("/").dropRight(1).mkString("/")
        case _ => curdir ++ "/" ++ ndir
    else
      val (p1, p2) = line.span(!_.isSpaceChar)
      val newV = if p1 == "dir" then s"$curdir/${p2.drop(1)}" else p1.toInt
      dirSizes(curdir) = newV :: dirSizes.getOrElse(curdir, Nil)
  
  val realSizes = HashMap[String, Int]()
  def sum(dir : String): Int = 
    val res = dirSizes.get(dir).get.foldLeft(0)((acc, x) => x match
      case s: String => acc + sum(s)
      case i: Int => acc + i
    )
    realSizes(dir) = res
    res

  sum("./")
  val storages = realSizes.toList.map(_._2).sorted
  
  override def run: Any =
    storages.filter(_ <= 100000).sum

  override def run2: Any =
    storages.dropWhile(70000000-realSizes("./") + _ <= 30000000).head