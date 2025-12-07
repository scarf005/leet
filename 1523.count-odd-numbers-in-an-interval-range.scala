package leet.`1523`

object Solution:
    def countOdds(low: Int, high: Int): Int = (high + 1) / 2 - low / 2

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (low = 3, high = 7, output = 3),
      (low = 8, high = 10, output = 1),
    ).foreach { case (low, high, expected) =>
        test(s"countOdds($low, $high) = $expected"):
            assertEquals(countOdds(low, high), expected)
    }
