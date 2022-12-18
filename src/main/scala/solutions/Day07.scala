package solutions
import utils.Utils.*

class Day07(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):
  // input : cmds
  // build adjlist from cmds (completely ignoring the "ls" commands)
  val tree = input
    .filter(!_.startsWith("$ ls"))
    // go through commands, using the current directory and the adjlist as accumulator
    .foldLeft(("./", Map[String, List[Any]]())) { case ((cdir, map), x) =>
      x match
        // if it's a (cd) command, change the directory and keep the map the same
        case s if s.startsWith("$") =>
          val ndir = s.substring(5) // drop "$ cd " from start
          (
            ndir match
              case "/"  => "./"
              case ".." => cdir.split("/").dropRight(1).mkString("/")
              case _    => s"$cdir/$ndir"
            ,
            map
          )
        // if it's not a command, check whether it's a directory or a file
        case s =>
          val (p1, p2) = s.span(!_.isSpaceChar)
          // if directory : add placeholder in adjlist. if file : add file size to adjlist
          val newV = if p1 == "dir" then s"$cdir/${p2.drop(1)}" else p1.toInt
          (cdir, map.updatedWith(cdir)(o => Some(newV :: o.getOrElse(Nil))))
    }
    ._2

  // get size of particular directory using the map from above
  def size(dir: String): Int =
    tree
      .get(dir)
      .get
      .foldLeft(0)((acc, x) =>
        x match
          case s: String => acc + size(s)
          case i: Int    => acc + i
      )

  val storages = tree.keySet.toList.map(size).sorted
  override def run =
    storages.filter(_ <= 100000).sum

  override def run2 =
    storages.dropWhile(70000000 - size("./") + _ <= 30000000).head
