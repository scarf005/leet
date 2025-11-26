package leet.`2435`

object Solution:
    inline val MOD = 1_000_000_007

    def numberOfPaths(grid: Array[Array[Int]], k: Int): Int =
        val (m, n) = (grid.size, grid(0).size)
        val dp = Array.fill(m + 1, n + 1, k)(0)
        dp(0)(1)(0) = 1

        for
            i <- 1 to m; j <- 1 to n
            value = grid(i - 1)(j - 1) % k
            r <- 0 until k
            prevMod = (r - value + k) % k
        do dp(i)(j)(r) = (dp(i - 1)(j)(prevMod) + dp(i)(j - 1)(prevMod)) % MOD

        dp(m)(n)(0)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (grid = "[[5,2,4],[3,0,5],[0,7,2]]", k = 3, res = 2),
      (grid = "[[0,0]]", k = 5, res = 1),
      (grid = "[[7,3,4,9],[2,3,6,2],[2,3,7,0]]", k = 1, res = 10),
    ).foreach { case (grid, k, res) =>
        test(s"numberOfPaths(${grid}, ${k}) = ${res}"):
            assertEquals(numberOfPaths(read[Array[Array[Int]]](grid), k), res)
    }
