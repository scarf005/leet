package leet.`3025`

object Solution:
    case class Point(y: Int, x: Int)
    case class Rect(p1: Point, p2: Point):
        infix inline def contains(p: Point): Boolean =
            (p.x - p1.x) * (p.x - p2.x) <= 0 && (p.y - p1.y) * (p.y - p2.y) <= 0

    def numberOfPairs(points: Array[Array[Int]]): Int =
        val pointSet = points.map { case Array(y, x) => Point(y, x) }.toSet

        extension (r: Rect)
            def isValid: Boolean = pointSet.forall(p => !(r contains p) || p == r.p1 || p == r.p2)

        extension (p1: Point)
            infix def isUpperLeftTo(p2: Point): Boolean = p1 != p2 && p1.x <= p2.x && p1.y >= p2.y

        val rects = for
            upperLeft <- pointSet
            lowerRight <- pointSet
            if upperLeft isUpperLeftTo lowerRight
        yield Rect(upperLeft, lowerRight)

        rects.count(_.isValid)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    test("Rect.contains"):
        assert(Rect(Point(6, 2), Point(2, 6)).contains(Point(4, 4)))
        assert(Rect(Point(2, 6), Point(6, 2)).contains(Point(4, 4)))

    type Input = Array[Array[Int]]
    List(
      (input = "[[1,1],[2,2],[3,3]]", expected = 0),
      (input = "[[6,2],[4,4],[2,6]]", expected = 2),
      (input = "[[3,1],[1,3],[1,1]]", expected = 2),
    ).foreach { case (input, expected) =>
        test(s"numberOfPairs($input) = $expected") {
            assertEquals(numberOfPairs(read[Input](input)), expected)
        }
    }
