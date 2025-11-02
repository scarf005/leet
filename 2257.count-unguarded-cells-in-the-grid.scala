package leet.`2257`

case class Pos(y: Int, x: Int):
    inline def +(that: Pos): Pos = Pos(this.y + that.y, this.x + that.x)

object Pos:
    val directions = List(Pos(-1, 0), Pos(1, 0), Pos(0, -1), Pos(0, 1))
    def unapply(arr: Array[Int]): Pos = Pos(arr(0), arr(1))

enum Cell(value: Int) extends java.lang.Enum[Cell]:
    case Empty extends Cell(1 << 0)
    case Wall extends Cell(1 << 1)
    case Guard extends Cell(1 << 2)
    case Seen extends Cell(1 << 3)

object Cell:
    val obstruct = java.util.EnumSet.of(Cell.Wall, Cell.Guard)

object Solution:
    def countUnguarded(m: Int, n: Int, guards: Array[Array[Int]], walls: Array[Array[Int]]): Int =
        val grid = Array.fill(m, n)(Cell.Empty)
        extension [A](grid: Array[Array[A]]) inline def apply(p: Pos) = grid(p.y)(p.x)
        extension (p: Pos)
            inline def inBounds = p.y >= 0 && p.y < m && p.x >= 0 && p.x < n
            inline def visible = p.inBounds && !Cell.obstruct.contains(grid(p))
            inline def +=(cell: Cell) = grid(p.y)(p.x) = cell

        walls.iterator.map(Pos.unapply).foreach(_ += Cell.Wall)
        guards.iterator.map(Pos.unapply).foreach(_ += Cell.Guard)
        for
            g <- guards.iterator.map(Pos.unapply)
            dir <- Pos.directions.iterator
        do Iterator.iterate(g + dir)(_ + dir).takeWhile(_.visible).foreach(_ += Cell.Seen)

        grid.iterator.flatten.count(_ == Cell.Empty)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    type Points = Array[Array[Int]]
    List(
      (
        input = (m = 4, n = 6, guards = "[[0,0],[1,1],[2,3]]", walls = "[[0,1],[2,2],[1,4]]"),
        expected = 7,
      ),
      (
        input = (m = 3, n = 3, guards = "[[1,1]]", walls = "[[0,1],[1,0],[2,1],[1,2]]"),
        expected = 4,
      ),
    ).foreach { case ((m, n, guards, walls), expected) =>
        test(s"countUnguarded($m, $n, $guards, $walls) = $expected"):
            assertEquals(countUnguarded(m, n, read[Points](guards), read[Points](walls)), expected)
    }
