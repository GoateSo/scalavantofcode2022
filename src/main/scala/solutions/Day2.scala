package solutions
import utils.Utils.*

class Day2(input : Seq[String]) extends Solution(input):
    val inputs = input.map(_.strip).toList

    val xs = inputs.map(str => 
        // 0:R, 1:P, 2:S
        (str(0)-'A', str(2)-'X')
    )

    def run(): Unit = 
        // (me-op format)
        // wins 0-2  1-0, 2-1,(-2,1,1)  (diff = 1 mod 3)(pts = 2(3))
        // loss 0-1, 1-2, 2-0 (-1,-1,2) (diff = 2 mod 3)(pts = 0(3))
        // tie  0-0  1-1  2-2 (0,0,0)   (diff = 0 mod 3)(pts = 1(3))
        // pts = ((diff%3+1)%3)*3
        val pts = xs.map((op, me) => 
            val res = me-op +% 3
            me+1 + (res+1)%3*3    
        )
        println(pts.sum)

    def run2(): Unit = 
        // 0 loss, 1 tie, 2 win (op->me format)
        // X(0): 0->1 1->2 2->0 (diff=2 mod 3)(pts = 0(3))
        // Y(1): 0->0 1->1 2->2 (diff=0 mod 3)(pts = 1(3))
        // Z(2): 0->2 1->0 2->1 (diff=1 mod 3)(pts = 2(3))
        // pts = 
        val pts = xs.map((op, res) => 
            val me = (op+res-1) +% 3
            me+1 + res*3
        )
        println(pts.sum)