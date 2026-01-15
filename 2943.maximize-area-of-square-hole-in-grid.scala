package leet.`2943`

import collection.mutable.ArraySeq

object Solution:
    extension (bars: ArraySeq.ofInt)
        inline def maxLen: Int =
            var (count, res) = (2, 2)
            for i <- 1 until bars.size do
                if bars(i) - bars(i - 1) == 1 then count += 1 else count = 2
                res = res max count
            res

    def maximizeSquareHoleArea(n: Int, m: Int, hBars: Array[Int], vBars: Array[Int]): Int =
        math.pow(hBars.sortInPlace.maxLen min vBars.sortInPlace.maxLen, 2).toInt

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (n = 2, m = 1, hBars = "[2,3]", vBars = "[2]", expected = 4),
      (n = 1, m = 1, hBars = "[2]", vBars = "[2]", expected = 4),
      (n = 2, m = 3, hBars = "[2,3]", vBars = "[2,4]", expected = 4),
    ).foreach { case (n, m, hBars, vBars, expected) =>
        test(s"maximizeSquareHoleArea($n, $m, $hBars, $vBars) = $expected"):
            assertEquals(
              maximizeSquareHoleArea(n, m, read[Array[Int]](hBars), read[Array[Int]](vBars)),
              expected,
            )
    }
