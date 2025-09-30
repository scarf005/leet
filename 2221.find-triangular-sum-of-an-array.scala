package leet.`2221`

import annotation.tailrec

object Solution:
    @tailrec def triangularSum(nums: Array[Int]): Int =
        if nums.size == 1 then nums.head else triangularSum(nums.sliding(2).map(_.sum % 10).toArray)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,2,3,4,5]", expected = 8),
      (input = "[5]", expected = 5),
      (input = "[2,6,6,5,5,3,3,8,6,4,3,3,5,1,0,1,3,6,9]", expected = 0),
    ).foreach { case (input, expected) =>
        test(s"triangularSum(${input}) = ${expected}"):
            assertEquals(triangularSum(read[Array[Int]](input)), expected)
    }
