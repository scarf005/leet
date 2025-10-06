package leet.`0778`

import collection.mutable
import annotation.tailrec
import util.boundary, boundary.break

case class Pos(y: Int, x: Int)

object Solution:
    def swimInWater(grid: Array[Array[Int]]): Int =
        val (min, max) = (grid.iterator.flatten.min, grid.iterator.flatten.max)
        val (width, height) = (grid(0).length, grid.length)

        extension (grid: Array[Array[Int]]) inline def apply(pos: Pos): Int = grid(pos.y)(pos.x)

        extension (p: Pos)
            inline def z = grid(p)
            inline def inBounds: Boolean =
                p.y >= 0 && p.y < height && p.x >= 0 && p.x < width
            inline def neighbors =
                List(Pos(p.y - 1, p.x), Pos(p.y + 1, p.x), Pos(p.y, p.x - 1), Pos(p.y, p.x + 1))
                    .filter(_.inBounds)

        given Ordering[Pos] = Ordering.by(-_.z)

        val (start, end) = (Pos(0, 0), Pos(height - 1, width - 1))

        def canReach(t: Int): Boolean = boundary:
            val visited = mutable.Set(start)
            val pq = mutable.PriorityQueue(start)
            while pq.nonEmpty do
                val current = pq.dequeue()
                if current.z > t then break(false)
                if current == end then break(true)

                val neighbor = current.neighbors.filterNot(visited).filter(_.z <= t)
                visited ++= neighbor
                pq ++= neighbor
            false

        @tailrec def binarySearch(low: Int, high: Int): Int =
            if low >= high then low
            else
                val mid = (low + high) / 2
                if canReach(mid) then binarySearch(low, mid)
                else binarySearch(mid + 1, high)
        binarySearch(min, max)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (grid = """[[0,2],[1,3]]""", expected = 3),
      (grid = """[[3,2],[0,1]]""", expected = 3),
      (
        grid = """[[0,1,2,3,4],[24,23,22,21,5],[12,13,14,15,16],[11,17,18,19,20],[10,9,8,7,6]]""",
        expected = 16,
      ),
      (
        grid = """[[0,1,2,3,4],[24,23,22,21,5],[12,13,14,15,16],[11,17,18,19,20],[10,9,8,7,6]]""",
        expected = 16,
      ),
    ).foreach { case (grid, expected) =>
        test(s"swimInWater($grid) = $expected"):
            assertEquals(swimInWater(read[Array[Array[Int]]](grid)), expected)
    }
