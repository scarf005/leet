package leet.`3397`

extension (inline n: Int)
    inline def clamp(inline low: Int, inline high: Int): Int =
        if n < low then low else if n > high then high else n

object Solution:
    def maxDistinctElements(nums: Array[Int], k: Int): Int = nums.sortInPlace
        .foldLeft(0, Int.MinValue) { case ((res, prev), n) =>
            val cur = (prev + 1).clamp(n - k, n + k)
            if cur > prev then (res + 1, cur) else (res, prev)
        }
        ._1

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = "[1,2,2,3,3,4]", k = 2, expected = 6),
      (nums = "[4,4,4,4]", k = 1, expected = 3),
    ).foreach { case (nums, k, expected) =>
        test(s"maxDistinctElements($nums, $k) = $expected"):
            assertEquals(maxDistinctElements(read[Array[Int]](nums), k), expected)
    }
