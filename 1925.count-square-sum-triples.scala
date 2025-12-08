package leet.`1925`

object Solution:
    def countTriples(n: Int): Int = (for
        c <- (1 to n).iterator
        a <- (1 until c).iterator
        b <- (1 until c).iterator
        if a * a + b * b == c * c
    yield 1).size

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (5, 2),
      (10, 4),
    ).foreach { case (input, expected) =>
        test(s"countTriples($input) = $expected"):
            assertEquals(countTriples(input), expected)
    }
