package solutions
import utils.Utils.*

class Day7(input : Seq[String]) extends Solution(input):
  // input : cmds
  val tree = input.filterNot(_.startsWith("$ ls"))
    .foldLeft(("./", Map.empty[String, List[Any]])){
      case ((cdir, map), x) => x match
        case s if s.startsWith("$") => 
          val ndir = s.substring(5) // drop "$ cd " from start
          (ndir match
            case "/" => "./"
            case ".." => cdir.split("/").dropRight(1).mkString("/")
            case _ => cdir ++ "/" ++ ndir
          , map)
        case s => 
          val (p1, p2) = s.span(!_.isSpaceChar)
          val newV = if p1 == "dir" then s"${cdir}/${p2.drop(1)}" else p1.toInt
          (cdir, map + (cdir -> (newV :: map.getOrElse(cdir, Nil))))
    }._2

  def sum(dir : String): Int =    
    tree.get(dir).get.foldLeft(0)((acc, x) => x match
      case s: String => acc + sum(s)
      case i: Int => acc + i
    )

  val rootCost=sum("./")
  val storages = tree.keySet.toList.map(sum).sorted
 
   override def run: Any =
    storages.filter(_ <= 100000).sum

  override def run2: Any =
    storages.dropWhile(70000000-sum("./") + _ <= 30000000).head