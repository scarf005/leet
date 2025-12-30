package leet.`0840`

import scala.util.boundary, boundary.break

object Solution:
    def numMagicSquaresInside(grid: Array[Array[Int]]): Int = boundary:
        val (rows, cols) = (grid.size, grid.head.size)
        if rows < 3 || cols < 3 then break(0)

        def isValid(r: Int, c: Int): Boolean = boundary:
            if grid(r + 1)(c + 1) != 5 then break(false)
            val seen = Array.fill(10)(false)
            for
                i <- 0 until 3
                j <- 0 until 3
                v = grid(r + i)(c + j)
            do
                if v < 1 || v > 9 || seen(v) then break(false)
                seen(v) = true

            val r0 = grid(r)(c) + grid(r)(c + 1) + grid(r)(c + 2) == 15
            val r2 = grid(r + 2)(c) + grid(r + 2)(c + 1) + grid(r + 2)(c + 2) == 15
            val c0 = grid(r)(c) + grid(r + 1)(c) + grid(r + 2)(c) == 15
            val c2 = grid(r)(c + 2) + grid(r + 1)(c + 2) + grid(r + 2)(c + 2) == 15
            val d1 = grid(r)(c) + grid(r + 1)(c + 1) + grid(r + 2)(c + 2) == 15
            val d2 = grid(r + 2)(c) + grid(r + 1)(c + 1) + grid(r)(c + 2) == 15

            r0 && r2 && c0 && c2 && d1 && d2

        (for
            i <- (0 to rows - 3).iterator
            j <- (0 to cols - 3).iterator
        yield isValid(i, j)).count(identity)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (grid = "[[4,3,8,4],[9,5,1,9],[2,7,6,2]]", expected = 1),
      (grid = "[[8]]", expected = 0),
    ).foreach { case (grid, expected) =>
        test(s"numMagicSquaresInside($grid)"):
            val gridArr = read[Array[Array[Int]]](grid)
            assertEquals(numMagicSquaresInside(gridArr), expected)
    }
