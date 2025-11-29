package leet.`3512`

object Solution:
    def minOperations(nums: Iterable[Int], k: Int): Int = nums.sum % k

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = "[3,9,7]", k = 5, output = 4),
      (nums = "[4,1,3]", k = 4, output = 0),
      (nums = "[3,2]", k = 6, output = 5),
    ).foreach { case (nums, k, expected) =>
        test(s"minOperations(${nums}, ${k}) = ${expected}"):
            assertEquals(
              minOperations(read[Vector[Int]](nums), k),
              expected,
            )
    }
