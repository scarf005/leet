package leet.`0070`

import scala.annotation.tailrec

object Solution:
    def climbStairs(n: Int): Int =
        @tailrec def go(m: Int, a: Int, b: Int): Int = m match
            case `n` => a
            case _   => go(m = m + 1, a = b, b = a + b)

        go(m = 1, a = 1, b = 2)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = 2, expected = 2),
      (input = 3, expected = 3),
      (input = 45, expected = 1836311903),
    ).foreach { case (input, expected) =>
        test(s"climbStairs($input) = $expected"):
            assertEquals(climbStairs(input), expected)
    }
