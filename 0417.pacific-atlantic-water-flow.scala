package leet.`0417`

import collection.mutable.Set
import math.Ordered.orderingToOrdered

case class Pos(y: Int, x: Int):
    inline def toList = List(y, x)
    inline def neighbors = List(Pos(y - 1, x), Pos(y + 1, x), Pos(y, x - 1), Pos(y, x + 1))

object Solution:
    def pacificAtlantic(heights: Array[Array[Int]]): List[List[Int]] =
        val (width, height) = (heights(0).length, heights.length)

        extension (heights: Array[Array[Int]]) inline def apply(p: Pos): Int = heights(p.y)(p.x)
        extension (p: Pos)
            inline def inBounds: Boolean =
                p.y >= 0 && p.y < heights.length && p.x >= 0 && p.x < heights(0).length
        given Ordering[Pos] = Ordering.by(heights(_))

        def dfs(start: Pos, visited: Set[Pos]): Unit =
            if !visited.contains(start) then
                visited.add(start)
                start.neighbors.iterator
                    .filter { p => p.inBounds && p >= start }
                    .foreach(dfs(_, visited))

        val pacific = Set.empty[Pos]
        val atlantic = Set.empty[Pos]
        for x <- 0 until width do
            dfs(Pos(0, x), pacific)
            dfs(Pos(height - 1, x), atlantic)
        for y <- 0 until height do
            dfs(Pos(y, 0), pacific)
            dfs(Pos(y, width - 1), atlantic)

        (pacific & atlantic).iterator.map(_.toList).toList

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*
    import collection.immutable.Set

    List(
      (
        input = """[[1,2,2,3,5],[3,2,3,4,4],[2,4,5,3,1],[6,7,1,4,5],[5,1,1,2,4]]""",
        expected = """[[0,4],[1,3],[1,4],[2,2],[3,0],[3,1],[4,0]]""",
      ),
      (input = """[[1]]""", expected = """[[0,0]]"""),
    ).foreach { case (input, expected) =>
        test(s"pacificAtlantic($input)"):
            assertEquals(
              pacificAtlantic(read[Array[Array[Int]]](input)).toSet,
              read[Set[List[Int]]](expected),
            )
    }
