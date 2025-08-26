package leet.`3000`

case class Rect(diag: Double, area: Int)
given Ordering[Rect] = Ordering.by(r => (r.diag, r.area))
object Rect:
    def apply(h: Int, w: Int): Rect = Rect(math.hypot(h, w), h * w)

object Solution:
    def areaOfMaxDiagonal(dimensions: Array[Array[Int]]): Int =
        dimensions.iterator.map { case Array(h, w) => Rect(h = h, w = w) }.max.area

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    test("cases"):
        assertEquals(areaOfMaxDiagonal(Array(Array(9, 3), Array(8, 6))), 48)
        assertEquals(areaOfMaxDiagonal(Array(Array(3, 4), Array(4, 3))), 12)
        assertEquals(
          areaOfMaxDiagonal(
            read[Array[Array[Int]]](
              "[[4,7],[8,9],[5,3],[6,10],[2,9],[3,10],[2,2],[5,8],[5,10],[5,6],[8,9],[10,7],[8,9],[3,7],[2,6],[5,1],[7,4],[1,10],[1,7],[6,9],[3,3],[4,6],[8,2],[10,6],[7,9],[9,2],[1,2],[3,8],[10,2],[4,1],[9,7],[10,3],[6,9],[9,8],[7,7],[5,7],[5,4],[6,5],[1,8],[2,3],[7,10],[3,9],[5,7],[2,4],[5,6],[9,5],[8,8],[8,10],[6,8],[5,1],[10,8],[7,4],[2,1],[2,7],[10,3],[2,5],[7,6],[10,5],[10,9],[5,7],[10,6],[4,3],[10,4],[1,5],[8,9],[3,1],[2,5],[9,10],[6,6],[5,10],[10,2],[6,10],[1,1],[8,6],[1,7],[6,3],[9,3],[1,4],[1,1],[10,4],[7,9],[4,5],[2,8],[7,9],[7,3],[4,9],[2,8],[4,6],[9,1],[8,4],[2,4],[7,8],[3,5],[7,6],[8,6],[4,7],[25,60],[39,52],[16,63],[33,56]]",
            ),
          ),
          2028,
        )
