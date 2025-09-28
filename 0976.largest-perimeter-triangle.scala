package leet.`0976`

object Solution:
    def largestPerimeter(nums: Array[Int]): Int =
        nums.sortInPlace()(using Ordering.Int.reverse)
        (for
            i <- 0 to nums.size - 3
            (x, y, z) = (nums(i), nums(i + 1), nums(i + 2))
            if x < y + z
        yield x + y + z).headOption.getOrElse(0)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[2,1,2]", expected = 5),
      (input = "[1,2,1,10]", expected = 0),
    ).foreach { case (input, expected) =>
        test(s"largestPerimeter(${input}) = ${expected}"):
            assertEquals(largestPerimeter(read[Array[Int]](input)), expected)
    }
