package leet.`0757`

extension (inline b: Boolean) inline def toInt: Int = if b then 1 else 0

object Solution:
    def intersectionSizeTwo(intervals: Array[Array[Int]]): Int =
        intervals.sortInPlaceBy { x => (x(1), -x(0)) }

        var res = 2
        var b = intervals.head(1)
        var a = b - 1
        for Array(l, r) <- intervals.tail if a < l do
            val noIntersection = l > b
            res += 1 + noIntersection.toInt
            a = if noIntersection then r - 1 else b
            b = r
        res

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[[1,3],[3,7],[8,9]]", expected = 5),
      (input = "[[1,3],[1,4],[2,5],[3,5]]", expected = 3),
      (input = "[[1,2],[2,3],[2,4],[4,5]]", expected = 5),
    ).foreach { case (input, expected) =>
        test(s"intersectionSizeTwo($input) = $expected"):
            assertEquals(intersectionSizeTwo(read[Array[Array[Int]]](input)), expected)
    }
