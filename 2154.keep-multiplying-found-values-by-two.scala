package leet.`2154`

import annotation.tailrec

object Solution:
    def findFinalValue(nums: Array[Int], original: Int): Int =
        @tailrec def go(n: Int): Int = if nums.contains(n) then go(n * 2) else n
        go(original)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = "[5,3,6,1,12]", original = 3, expected = 24),
      (nums = "[2,7,9]", original = 4, expected = 4),
    ).foreach { case (nums, original, expected) =>
        test(s"findFinalValue($nums, $original) = $expected"):
            assertEquals(findFinalValue(read[Array[Int]](nums), original), expected)
    }
