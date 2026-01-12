package leet.`1266`

object Solution:
    def minTimeToVisitAllPoints(points: Array[Array[Int]]): Int =
        points.indices.dropRight(1).foldLeft(0) { (acc, i) =>
            val (l, r) = (points(i), points(i + 1))
            val (x1, y1, x2, y2) = (l(0), l(1), r(0), r(1))
            acc + ((x2 - x1).abs max (y2 - y1).abs)
        }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (points = "[[1,1]]", expected = 0),
      (points = "[[1,1],[3,4],[-1,0]]", expected = 7),
      (points = "[[3,2],[-2,2]]", expected = 5),
    ).foreach { case (points, expected) =>
        test(s"minTimeToVisitAllPoints(${points}) = ${expected}"):
            assertEquals(minTimeToVisitAllPoints(read[Array[Array[Int]]](points)), expected)
    }
