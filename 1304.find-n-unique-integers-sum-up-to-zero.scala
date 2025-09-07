package leet.`1304`

object Solution:
    def sumZero(n: Int): Array[Int] = (0 until n).iterator.map(i => i * 2 - n + 1).toArray

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(5, 3, 1).foreach { case (input) =>
        test(s"sumZero($input)"):
            val res = sumZero(input)
            assertEquals(res.sum, 0)
            assertEquals(res.distinct.size, input)
    }
