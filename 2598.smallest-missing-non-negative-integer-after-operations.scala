package leet.`2598`

extension (xs: Array[Int])
    inline def sub1(index: Int) = if xs(index) == 0 then false
    else
        xs(index) -= 1; true

object Solution:
    def findSmallestInteger(nums: Array[Int], value: Int): Int =
        val freq = Array.ofDim[Int](value)
        for n <- nums do freq((n % value + value) % value) += 1

        Iterator.from(0).find { i => !freq.sub1(i % value) }.get

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = "[1,-10,7,13,6,8]", value = 5, expected = 4),
      (nums = "[1,-10,7,13,6,8]", value = 7, expected = 2),
      (nums = "[3,0,3,2,4,2,1,1,0,4]", value = 5, expected = 10),
    ).foreach { case (nums, value, expected) =>
        test(s"findSmallestInteger($nums, $value) = $expected"):
            assertEquals(findSmallestInteger(read[Array[Int]](nums), value), expected)
    }
