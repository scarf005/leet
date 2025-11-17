package leet.`1437`

import annotation.tailrec

object Solution:
    def kLengthApart(nums: Array[Int], k: Int): Boolean =
        @tailrec def go(i: Int, lastOne: Int): Boolean =
            if i >= nums.size then true
            else if nums(i) == 1 then
                if lastOne >= 0 && i - lastOne - 1 < k then false
                else go(i + 1, i)
            else go(i + 1, lastOne)
        go(0, -1)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
        (nums = "[1,0,0,0,1,0,0,1]", k = 2, expected = true),
        (nums = "[1,0,0,1,0,1]", k = 2, expected = false),
        (nums = "[1,0,0,0,1,0,0,1,0]", k = 2, expected = true),
      (nums = "[1,1,1,0]", k = 3, expected = false),
    ).foreach { case (nums, k, expected) =>
        test(s"kLengthApart($nums, $k) = $expected"):
            assertEquals(kLengthApart(read[Array[Int]](nums), k), expected)
    }
