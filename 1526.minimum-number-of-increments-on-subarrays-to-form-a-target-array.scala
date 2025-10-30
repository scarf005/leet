package leet.`1526`

object Solution:
    def minNumberOperations(xs: Array[Int]): Int =
        xs.head + (1 until xs.size).foldLeft(0) { (a, b) => a + ((xs(b) - xs(b - 1)) max 0) }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,2,3,2,1]", expected = 3),
      (input = "[3,1,1,2]", expected = 4),
      (input = "[3,1,5,4,2]", expected = 7),
    ).foreach { case (input, expected) =>
        test(s"minNumberOperations($input) = $expected"):
            assertEquals(minNumberOperations(read[Array[Int]](input)), expected)
    }
