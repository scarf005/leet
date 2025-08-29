package leet.`3021`

object Solution:
    def flowerGame(n: Long, m: Long): Long = n * m / 2

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = (3L, 2L), expected = 3L),
      (input = (1L, 1L), expected = 0L),
    ).foreach { case ((a, b), expected) =>
        test(s"flowerGame($a, $b)"):
            assertEquals(flowerGame(a, b), expected)
    }
