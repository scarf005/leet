package leet.`2536`

object Solution:
    def rangeAddQueries(n: Int, queries: Array[Array[Int]]): Array[Array[Int]] =
        val grid = Array.ofDim[Int](n, n)
        for
            Array(y1, x1, y2, x2) <- queries
            y <- y1 to y2
        do
            grid(y)(x1) += 1
            if (x2 + 1) < n then grid(y)(x2 + 1) -= 1

        for
            y <- 0 until n
            x <- 1 until n
        do grid(y)(x) += grid(y)(x - 1)
        grid

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (n = 3, queries = "[[1,1,2,2],[0,0,1,1]]", output = "[[1,1,0],[1,2,1],[0,1,1]]"),
      (n = 2, queries = "[[0,0,1,1]]", output = "[[1,1],[1,1]]"),
    ).foreach { case (n, queries, output) =>
        test(s"rangeAddQueries($n, $queries) = $output"):
            assertEquals(
              (rangeAddQueries(n, read[Array[Array[Int]]](queries)).map(_.toSeq).toSeq),
              read[Seq[Seq[Int]]](output),
            )
    }
