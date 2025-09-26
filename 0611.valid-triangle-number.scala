package leet.`0611`

object Solution:
    def triangleNumber(nums: Array[Int]): Int =
        nums.sortInPlace()
        var count = 0
        for i <- nums.size - 1 to 2 by -1 do
            var (left, right) = (0, i - 1)
            while left < right do
                if nums(left) + nums(right) > nums(i) then
                    count += right - left
                    right -= 1
                else left += 1
        count

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[2,2,3,4]", expected = 3),
      (input = "[4,2,3,4]", expected = 4),
    ).foreach { case (input, expected) =>
        test(s"triangleNumber(${input}) = ${expected}"):
            assertEquals(triangleNumber(read[Array[Int]](input)), expected)
    }
