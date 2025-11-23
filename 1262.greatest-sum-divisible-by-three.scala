package leet.`1262`

extension (xs: Array[Int])
    inline def safeTake(n: Int) = if xs.size >= n then xs.take(n) else Array.empty[Int]

object Solution:
    def maxSumDivThree(nums: Array[Int]): Int =
        val rs = nums.groupBy(_ % 3).withDefaultValue(Array.empty[Int])
        val `1s` = rs(1).sorted
        val `2s` = rs(2).sorted
        val total = nums.sum
        (for i <- 0 to 2; j <- 0 to 2
        yield total - (`1s`.safeTake(i).sum + `2s`.safeTake(j).sum)).filter(_ % 3 == 0).max

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[3,6,5,1,8]", expected = 18),
      (input = "[4]", expected = 0),
      (input = "[1,2,3,4,4]", expected = 12),
    ).foreach { case (input, expected) =>
        test(s"maxSumDivThree($input) = $expected"):
            assertEquals(maxSumDivThree(read[Array[Int]](input)), expected)
    }
