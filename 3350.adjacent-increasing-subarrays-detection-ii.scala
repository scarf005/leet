package leet.`3350`

object Solution:
    def maxIncreasingSubarrays(nums: List[Int]): Int =
        val vec = nums.toVector
        var (prev, cur, res) = (0, 1, 0)
        for i <- 1 until vec.size do
            if vec(i) > vec(i - 1) then cur += 1
            else
                prev = cur
                cur = 1
            res = res max (prev min cur) max (cur / 2)
        res

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[2,5,7,8,9,2,3,4,3,1]", expected = 3),
      (input = "[1,2,3,4,4,4,4,5,6,7]", expected = 2),
      (input = "[-15,19]", expected = 1),
    ).foreach { case (input, expected) =>
        test(s"maxIncreasingSubarrays($input) = $expected"):
            assertEquals(maxIncreasingSubarrays(read[List[Int]](input)), expected)
    }
