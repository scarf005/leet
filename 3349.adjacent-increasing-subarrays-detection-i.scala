package leet.`3349`

object Solution:
    def hasIncreasingSubarrays(nums: List[Int], k: Int): Boolean =
        var (prev, cur) = (0, 1)
        (1 until nums.size).exists { i =>
            if nums(i) > nums(i - 1) then cur += 1
            else
                prev = cur
                cur = 1

            (cur >= k && prev >= k) || (cur >= k * 2)
        }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = "[2,5,7,8,9,2,3,4,3,1]", k = 3, expected = true),
      (nums = "[1,2,3,4,4,4,4,5,6,7]", k = 5, expected = false),
    ).foreach { case (nums, k, expected) =>
        test(s"hasIncreasingSubarrays($nums, $k) = $expected"):
            assertEquals(hasIncreasingSubarrays(read[List[Int]](nums), k), expected)
    }
