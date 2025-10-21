package leet.`3346`

extension (xs: Array[Int]) inline def get(i: Int) = if i >= 0 && i < xs.length then xs(i) else 0

object Solution:
    def maxFrequency(nums: Array[Int], k: Int, numOperations: Int): Int =
        val maxVal = nums.max + k + 2
        val count = Array.fill(maxVal)(0)

        for v <- nums do count(v) += 1
        for i <- 1 until maxVal do count(i) += count(i - 1)

        (0 until maxVal).iterator
            .map: i =>
                val total = count.get(i + k) - count.get(i - k - 1)
                val freq = count(i) - count.get(i - 1)
                freq + (numOperations min (total - freq))
            .maxOption
            .getOrElse(0)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = (nums = "[1,4,5]", k = 1, numOperations = 2), expected = 2),
      (input = (nums = "[5,11,20,20]", k = 5, numOperations = 1), expected = 2),
      (input = (nums = "[2,70,73]", k = 39, numOperations = 2), expected = 2),
      (input = (nums = "[2,49]", k = 97, numOperations = 0), expected = 1),
      (input = (nums = "[88,53]", k = 27, numOperations = 2), expected = 2),
    ).foreach { case ((nums, k, numOperations), expected) =>
        test(s"maxFrequency($nums, $k, $numOperations) = $expected"):
            assertEquals(maxFrequency(read[Array[Int]](nums), k, numOperations), expected)
    }
