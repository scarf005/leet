package leet.`3354`

object Solution:
    def countValidSelections(nums: Array[Int]): Int =
        var (total, left, sum) = (0, 0, nums.sum)
        for (n, i) <- nums.iterator.zipWithIndex do
            left += n
            if n == 0 then
                math.abs(sum - left * 2) match
                    case 0 => total += 2
                    case 1 => total += 1
                    case _ =>
        total

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,0,2,0,3]", expected = 2),
      (input = "[2,3,4,0,4,1,0]", expected = 0),
    ).foreach { case (input, expected) =>
        test(s"countValidSelections($input) = $expected"):
            assertEquals(countValidSelections(read[Array[Int]](input)), expected)
    }
