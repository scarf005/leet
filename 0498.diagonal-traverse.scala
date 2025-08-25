package leet.`0498`

enum Dir:
    case Up, Down

case class Cursor(y: Int, x: Int, dir: Dir)

object Cursor:
    val zero: Cursor = Cursor(0, 0, Dir.Up)

object Solution:
    import Dir.*

    def findDiagonalOrder(mat: Array[Array[Int]]): Array[Int] =
        if mat.isEmpty || mat(0).isEmpty then return Array.empty

        val (maxY, maxX) = (mat.size - 1, mat(0).size - 1)
        val totalElements = mat.length * mat(0).length

        def getNext(c: Cursor): Cursor = c match
            case Cursor(y, x, Up) =>
                if x == maxX then Cursor(y + 1, x, Down) // Right wall: ↓↙
                else if y == 0 then Cursor(y, x + 1, Down) // Top wall: →↙
                else Cursor(y - 1, x + 1, Up) // ↗

            case Cursor(y, x, Down) =>
                if y == maxY then Cursor(y, x + 1, Up) // Bottom wall: →↖
                else if x == 0 then Cursor(y + 1, x, Up) // Left wall: ↓↖
                else Cursor(y + 1, x - 1, Down) // ↙

        Iterator
            .iterate(Cursor.zero)(getNext)
            .take(totalElements)
            .map { case Cursor(y, x, _) => mat(y)(x) }
            .toArray

import munit.FunSuite
class Suite extends FunSuite:
    import Solution.*

    test("cases"):
        assertEquals(
          findDiagonalOrder(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))).toSeq,
          Seq(1, 2, 4, 7, 5, 3, 6, 8, 9),
        )
        assertEquals(findDiagonalOrder(Array(Array(1, 2), Array(3, 4))).toSeq, Seq(1, 2, 3, 4))

    test("extra"):
        assertEquals(findDiagonalOrder(Array(Array(1))).toSeq, Seq(1))
        assertEquals(findDiagonalOrder(Array(Array(1), Array(2), Array(3))).toSeq, Seq(1, 2, 3))
        assertEquals(findDiagonalOrder(Array(Array(1, 2, 3))).toSeq, Seq(1, 2, 3))
        assertEquals(
          findDiagonalOrder(
            Array(
              Array(1, 2, 3, 4),
              Array(5, 6, 7, 8),
              Array(9, 10, 11, 12),
            ),
          ).toSeq,
          Seq(1, 2, 5, 9, 6, 3, 4, 7, 10, 11, 8, 12),
        )
