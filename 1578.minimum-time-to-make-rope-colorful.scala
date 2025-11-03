package leet.`1578`

object Solution:
    def minCost(colors: String, neededTime: Array[Int]): Int =
        var longest = 0
        var total = 0
        for curr <- 1 until colors.size do
            if colors(longest) == colors(curr) then
                total += neededTime(longest) min neededTime(curr)
                if neededTime(longest) < neededTime(curr) then longest = curr
            else longest = curr
        total

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (colors = "abaac", neededTime = "[1,2,3,4,5]", output = 3),
      (colors = "abc", neededTime = "[1,2,3]", output = 0),
      (colors = "aabaa", neededTime = "[1,2,3,4,1]", output = 2),
      (colors = "aaabbbabbbb", neededTime = "[3,5,10,7,5,3,5,5,4,8,1]", output = 26),
    ).foreach { case (colors, neededTime, expected) =>
        test(s"minCost($colors, $neededTime) = $expected"):
            assertEquals(minCost(colors, read[Array[Int]](neededTime)), expected)
    }
