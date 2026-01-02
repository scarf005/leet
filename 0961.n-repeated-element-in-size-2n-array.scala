package leet.`0961`

object Solution:
    def repeatedNTimes(nums: Array[Int]): Int =
        nums.groupMapReduce(identity)(_ => 1)(_ + _).find { _._2 == nums.size / 2 }.get._1

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,2,3,3]", expected = 3),
      (input = "[2,1,2,5,3,2]", expected = 2),
      (input = "[5,1,5,2,5,3,5,4]", expected = 5),
    ).foreach { case (input, expected) =>
        test(s"repeatedNTimes($input)"):
            assertEquals(repeatedNTimes(read[Array[Int]](input)), expected)
    }
