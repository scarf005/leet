package leet.`3005`

object Solution:
    def maxFrequencyElements(nums: Array[Int]): Int =
        val freq = nums.groupMapReduce(identity)(_ => 1)(_ + _).values
        val max = freq.max
        max * freq.count(_ == max)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = "[1,2,2,3,1,4]", expected = 4),
      (nums = "[1,2,3,4,5]", expected = 5),
    ).foreach { case (nums, expected) =>
        test(s"maxFrequencyElements(${nums}) = ${expected}"):
            assertEquals(maxFrequencyElements(read[Array[Int]](nums)), expected)
    }
