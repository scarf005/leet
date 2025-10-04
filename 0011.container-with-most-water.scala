package leet.`0011`

object Solution:
    def maxArea(height: Array[Int]): Int =
        var res = 0
        var right = height.length - 1
        var left = 0
        while left < right do
            val lh = height(left)
            val rh = height(right)
            res = res max (rh min lh) * (right - left)
            if lh < rh then left += 1 else right -= 1
        res

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,8,6,2,5,4,8,3,7]", expected = 49),
      (input = "[1,1]", expected = 1),
    ).foreach { case (input, expected) =>
        test(s"maxArea($input) = $expected"):
            assertEquals(maxArea(read[Array[Int]](input)), expected)
    }
