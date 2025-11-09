package leet.`2169`

import annotation.tailrec

object Solution:
    @tailrec def countOperations(a: Int, b: Int, count: Int = 0): Int =
        if a == 0 || b == 0 then count else countOperations(b, a % b, count + a / b)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = (num1 = 2, num2 = 3), expected = 3),
      (input = (num1 = 10, num2 = 10), expected = 1),
    ).foreach { case ((num1, num2), expected) =>
        test(s"countOperations($num1, $num2) = $expected"):
            assertEquals(countOperations(num1, num2), expected)
    }
