package leet.`1611`

object Solution:
    def minimumOneBitOperations(n: Int): Int =
        Vector(16, 8, 4, 2, 1).foldLeft(n) { (a, b) => a ^ (a >> b) }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = 3, expected = 2),
      (input = 6, expected = 4),
    ).foreach { case (input, expected) =>
        test(s"minimumOneBitOperations($input) = $expected"):
            assertEquals(minimumOneBitOperations(input), expected)
    }
