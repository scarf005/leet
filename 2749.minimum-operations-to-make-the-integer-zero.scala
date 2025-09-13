package leet.`2749`

import java.lang.Long.bitCount

object Solution:
    def makeTheIntegerZero(a: Long, b: Long): Int =
        val res = (1 to 60).find { k =>
            val d = a - b * k
            bitCount(d) <= k && k <= d
        }
        res.getOrElse(-1)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    List(
      (input = (3L, -2L), expected = 3),
      (input = (5L, 7L), expected = -1),
    ).foreach { case ((a, b), expected) =>
        test(s"makeTheIntegerZero($a, $b) = $expected"):
            assertEquals(makeTheIntegerZero(a, b), expected)
    }
