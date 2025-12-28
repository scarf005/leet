package leet.`1351`

import annotation.tailrec

object Solution:
    def countNegatives(grid: Array[Array[Int]]): Int =
        val (m, n) = (grid.size, grid.head.size)

        @inline def go(i: Int, j: Int, acc: Int = 0): Int =
            if i < 0 || j >= n then acc
            else if grid(i)(j) < 0 then go(i - 1, j, acc + n - j)
            else go(i, j + 1, acc)

        go(m - 1, 0)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[[4,3,2,-1],[3,2,1,-1],[1,1,-1,-2],[-1,-1,-2,-3]]", expected = 8),
      (input = "[[3,2],[1,0]]", expected = 0),
    ).foreach { case (input, expected) =>
        test(s"countNegatives($input) = $expected"):
            assertEquals(countNegatives(read[Array[Array[Int]]](input)), expected)
    }
