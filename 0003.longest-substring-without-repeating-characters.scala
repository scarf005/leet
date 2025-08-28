package leet.`0003`

object Solution:
    def lengthOfLongestSubstring(s: String): Int =
        var left = 0
        val seen = scala.collection.mutable.Set.empty[Char]
        var maxLen = 0
        for c <- s do
            while seen(c) do
                seen.remove(s(left))
                left += 1
            seen.add(c)
            maxLen = math.max(maxLen, seen.size)
        maxLen

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
