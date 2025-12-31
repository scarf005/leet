package leet.`1970`

import scala.util.boundary, boundary.break

object Solution:
    def latestDayToCross(row: Int, col: Int, cells: Array[Array[Int]]): Int = boundary:
        val dsu = UnionFind(row * col + 2)
        val grid = Array.fill(row, col)(0)
        val dirs = Vector((0, 1), (0, -1), (1, 0), (-1, 0))
        val (begin, end) = (0, row * col + 1)
        for
            i <- cells.size - 1 to 0 by -1
            cell = cells(i)
            r = cell(0) - 1
            c = cell(1) - 1
        do
            grid(r)(c) = 1
            val id1 = r * col + c + 1
            for
                (dr, dc) <- dirs
                nr = r + dr
                nc = c + dc
                if nr >= 0 && nr < row && nc >= 0 && nc < col && grid(nr)(nc) == 1
                id2 = nr * col + nc + 1
            do dsu.union(id1, id2)

            if r == 0 then dsu.union(0, id1)
            if r == row - 1 then dsu.union(end, id1)
            if dsu.find(begin) == dsu.find(end) then break(i)
        -1

class UnionFind(n: Int):
    private val parent = (0 until n).toArray
    private val size = Array.fill(n)(1)

    def find(x: Int): Int =
        if parent(x) != x then parent(x) = find(parent(x))
        parent(x)

    def union(x: Int, y: Int): Unit =
        val rootX = find(x)
        val rootY = find(y)
        if rootX != rootY then
            if size(rootX) > size(rootY) then parent(rootY) = rootX
            else if size(rootX) < size(rootY) then parent(rootX) = rootY
            else
                parent(rootY) = rootX
                size(rootX) += size(rootY)

    inline def connected(x: Int, y: Int): Boolean = find(x) == find(y)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (
        row = 2,
        col = 2,
        cells = "[[1,1],[2,1],[1,2],[2,2]]",
        expected = 2,
      ),
      (
        row = 2,
        col = 2,
        cells = "[[1,1],[1,2],[2,1],[2,2]]",
        expected = 1,
      ),
      (
        row = 3,
        col = 3,
        cells = "[[1,2],[2,1],[3,3],[2,2],[1,1],[1,3],[2,3],[3,2],[3,1]]",
        expected = 3,
      ),
    ).foreach { case (row = row, col = col, cells = cells, expected = expected) =>
        test(s"latestDayToCross($row, $col, $cells)"):
            val cellsArr = read[Array[Array[Int]]](cells)
            assertEquals(latestDayToCross(row, col, cellsArr), expected)
    }
