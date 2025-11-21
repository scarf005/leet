package leet.`1930`

object Solution:
    def countPalindromicSubsequence(s: String): Int = (for c <- ('a' to 'z').iterator yield
        val (first, last) = (s.indexOf(c), s.lastIndexOf(c))
        if first >= 0 && last - first >= 2 then s.slice(first + 1, last).distinct.size else 0
    ).sum

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "aabca", expected = 3),
      (input = "adc", expected = 0),
      (input = "bbcbaba", expected = 4),
    ).foreach { case (input, expected) =>
        test(s"countPalindromicSubsequence($input) = $expected"):
            assertEquals(countPalindromicSubsequence(input), expected)
    }
