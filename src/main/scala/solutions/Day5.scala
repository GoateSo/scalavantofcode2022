package solutions
import utils.Utils.*

class Day5(input : Seq[String]) extends Solution(input):
  val (fst, snd) = input.span(!_.isEmpty)         // remove empty line w/ pattern matching
  
  // 1st half, read the stacks of values (every 4th value should either be space or letter)
  val cols = fst.map(_.toIndexedSeq)             // convert to 2d seq of chars
          .columns.drop(1)                       // get the columns, and remove the first (only parens and space) 
          .sliding(1,4).map(_.flatten)           // get every 4th character from each
          .map(_.dropWhile(!_.isLetter))         // keep only the letters, throwing the spaces on top out
          .toList
  // 2nd half, read inputs in format "move x from c1 to c2"
  val moves = snd.drop(1).map(_.split(" ").toList) // remove empty line & split on spaces
          .map({                                   // convert to list of interger inputs
            case List("move",x,"from",c1,"to",c2) => List(x.toInt, c1.toInt-1, c2.toInt-1)
            case _ => Nil                          // should never occur
          })

  def runMoves(dropBlocks: Seq[Char] => Seq[Char]): Seq[Seq[Char]] = {
    moves.foldLeft(cols)((xs,move) => { // go through each move, keeping track of current stacks, returning finished stacks
      val List(n,c1,c2) = move    
      val (sub, rem) = xs(c1).splitAt(n) // split at the point of removal
      xs.updated(c1, rem)                // update the stacks(remove from c1, add to c2)
        .updated(c2, dropBlocks(sub) ++ xs(c2))
    })
  }
  // part 1: 1 at a time (so in reverse order)
  override def run =
    runMoves(_.reverse).map(_.head).mkString

  // part 2: all at once (so in normal order)
  override def run2 = 
    runMoves(identity).map(_.head).mkString
