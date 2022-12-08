package solutions
import utils.Utils.*
import util.chaining.scalaUtilChainingOps
import scala.collection.mutable.{ArrayBuffer, HashMap}

class Day7(input : Seq[String]) extends Solution(input):
  // input : cmds
  val tree = input.filterNot(_.startsWith("$ ls"))
    .foldLeft(("./", Map.empty[String, List[Any]])){
      (acc, x) => x match
        case s if s.startsWith("$") => 
          val ndir = s.drop(2).dropWhile(_ != ' ').strip
          (ndir match
            case "/" => "./"
            case ".." => acc._1.split("/").dropRight(1).mkString("/")
            case _ => acc._1 ++ "/" ++ ndir
          , acc._2)
        case s => 
          val (p1, p2) = s.span(!_.isSpaceChar)
          val newV = if p1 == "dir" then s"${acc._1}/${p2.drop(1)}" else p1.toInt
          (acc._1, acc._2 + (acc._1 -> (newV :: acc._2.getOrElse(acc._1, Nil))))
    }._2

  val realSizes = HashMap[String, Int]()
  def sum(dir : String): Int =    
    tree.get(dir).get.foldLeft(0)((acc, x) => x match
      case s: String => acc + sum(s)
      case i: Int => acc + i
    ).tap(realSizes(dir) = _)

  sum("./")
  val storages = realSizes.toList.map(_._2).sorted
  
  override def run: Any =
    storages.filter(_ <= 100000).sum

  override def run2: Any =
    storages.dropWhile(70000000-realSizes("./") + _ <= 30000000).head