package leet.`0812`

object Solution:
    def largestTriangleArea(points: Array[Array[Int]]): Double = points
        .combinations(3)
        .map { case Array(Array(x1, y1), Array(x2, y2), Array(x3, y3)) =>
            0.5 * Math.abs((x1 * y2 + x2 * y3 + x3 * y1) - (y1 * x2 + y2 * x3 + y3 * x1))
        }
        .max

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[[0,0],[0,1],[1,0],[0,2],[2,0]]", expected = 2.0),
      (input = "[[1,0],[0,0],[0,1]]", expected = 0.5),
    ).foreach { case (input, expected) =>
        test(s"largestTriangleArea(${input}) = ${expected}"):
            assertEquals(largestTriangleArea(read[Array[Array[Int]]](input)), expected)
    }
