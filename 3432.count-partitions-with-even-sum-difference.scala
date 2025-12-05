package leet.`3432`

object Solution:
    def countPartitions(nums: Array[Int]): Int = (nums.size - 1) * (1 - nums.sum % 2)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = Array(10, 10, 3, 7, 6), output = 4),
      (nums = Array(1, 2, 2), output = 0),
      (nums = Array(2, 4, 6, 8), output = 3),
    ).foreach { case (input, expected) =>
        test(s"countPartitions(${write(input)}) = $expected"):
            assertEquals(countPartitions(input), expected)
    }
