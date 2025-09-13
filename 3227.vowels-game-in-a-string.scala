package leet.`3227`

object Solution:
    def doesAliceWin(s: String): Boolean = s.exists("aeiou".contains)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    List(
      (input = "leetcoder", expected = true),
      (input = "bbcd", expected = false),
    ).foreach { case (input, expected) =>
        test(s"doesAliceWin($input)"):
            assertEquals(doesAliceWin(input), expected)
    }
