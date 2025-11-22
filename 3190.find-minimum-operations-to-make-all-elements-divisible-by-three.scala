package leet.`3190`

object Solution:
    def minimumOperations(nums: Array[Int]): Int = nums.count { _ % 3 != 0 }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,2,3,4]", expected = 3),
      (input = "[3,6,9]", expected = 0),
    ).foreach { case (input, expected) =>
        test(s"minimumOperations($input) = $expected"):
            assertEquals(minimumOperations(read[Array[Int]](input)), expected)
    }
