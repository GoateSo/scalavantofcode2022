package solutions

class Day08(input: Seq[String], isSample: Boolean)
    extends Solution(input, isSample):
  val ts = input.toIndexedSeq.map(_.toIndexedSeq)
  val xs =
    for i <- 0 until ts.size yield for j <- 0 until ts(i).size yield
      // directional views
      val up = (i - 1 to 0 by -1) map (ts(_)(j))
      val down = (i + 1 until ts.size) map (ts(_)(j))
      val left = (j - 1 to 0 by -1) map (ts(i)(_))
      val right = (j + 1 until ts(i).size) map (ts(i)(_))
      (ts(i)(j), List(up, down, left, right))

  override def run =
    // count # of grid positions with at least one un-obstructed view
    xs.flatten.count { case (x, views) =>
      views.exists(v => v.isEmpty || v.max < x)
    }

  override def run2 =
    // compute all viewing distance, taking their product, and getting the maximum of those
    xs.flatten.map { case (x, views) =>
      views.map(v => Math.min(v.size, v.takeWhile(_ < x).size + 1)).product
    }.max
