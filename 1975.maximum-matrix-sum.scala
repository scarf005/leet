package leet.`1975`

object Solution:
    def maxMatrixSum(matrix: Array[Array[Int]]): Long =
        var totalSum = 0L
        var minAbsVal = Int.MaxValue
        var negativeCount = 0

        for
            row <- matrix
            value <- row
        do
            totalSum += math.abs(value)
            if value < 0 then negativeCount += 1
            minAbsVal = minAbsVal min math.abs(value)

        if negativeCount % 2 != 0 then totalSum -= 2 * minAbsVal
        totalSum

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[[1,-1],[-1,1]]", expected = 4L),
      (input = "[[1,2,3],[-1,-2,-3],[1,2,3]]", expected = 16L),
    ).foreach { case (input, expected) =>
        test(s"maxMatrixSum($input) = $expected"):
            assertEquals(maxMatrixSum(read[Array[Array[Int]]](input)), expected)
    }
