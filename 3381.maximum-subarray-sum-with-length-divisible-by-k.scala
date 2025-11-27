package leet.`3381`

object Solution:
    def maxSubarraySum(nums: Array[Int], k: Int): Long =
        var maxSum = Long.MinValue
        val kSum = Array.fill(k)(Long.MaxValue / 2)
        kSum(k - 1) = 0L
        var prefixSum = 0L
        for (n, i) <- nums.zipWithIndex do
            prefixSum += n
            maxSum = maxSum max (prefixSum - kSum(i % k))
            kSum(i % k) = kSum(i % k) min prefixSum
        maxSum

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = "[1,2]", k = 1, res = 3L),
      (nums = "[-1,-2,-3,-4,-5]", k = 4, res = -10L),
      (nums = "[-5,1,2,-3,4]", k = 2, res = 4L),
    ).foreach { case (nums, k, res) =>
        test(s"maxSubarraySum(${nums}, ${k}) = ${res}"):
            assertEquals(maxSubarraySum(read[Array[Int]](nums), k), res)
    }
