package leet.`0003`

import collection.mutable.Set

object Solution:
    def lengthOfLongestSubstring(s: String): Int =
        var left = 0
        val seen = Set.empty[Char]
        s.foldLeft(0) { (maxLen, c) =>
            while seen(c) do
                seen.remove(s(left))
                left += 1
            seen.add(c)
            math.max(maxLen, seen.size)
        }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    List(
      (input = "abcabcbb", expected = 3),
      (input = "bbbbb", expected = 1),
      (input = "pwwkew", expected = 3),
    ).foreach { case (input, expected) =>
        test(s"lengthOfLongestSubstring($input)"):
            assertEquals(lengthOfLongestSubstring(input), expected)
    }
