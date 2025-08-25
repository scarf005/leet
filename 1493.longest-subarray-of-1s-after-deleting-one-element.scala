package leet.`1493`

import scala.math.max

object Solution:
    def longestSubarray(nums: Array[Int]): Int =
        var left = 0
        var zeros = 0
        var maxLen = 0

        for (right <- nums.indices) {
            if (nums(right) == 0) zeros += 1

            // shrink left cursor till at most one zero in the window
            while (zeros > 1) {
                if (nums(left) == 0) zeros -= 1
                left += 1
            }

            // right - left gives the length of 1 without one 0
            maxLen = maxLen `max` right - left
        }

        maxLen
