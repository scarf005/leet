package leet.`0474`

object Solution:
    def findMaxForm(strs: Array[String], M: Int, N: Int): Int =
        val dp = Array.ofDim[Int](M + 1, N + 1)
        val nums = strs.iterator.map: x =>
            val zeros = x.count(_ == '0')
            (zeros, x.size - zeros)
        for
            (z, o) <- nums
            i <- M to z by -1
            j <- N to o by -1
        do dp(i)(j) = dp(i)(j) max (dp(i - z)(j - o) + 1)
        dp(M)(N)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (strs = """["10","0001","111001","1","0"]""", m = 5, n = 3, expected = 4),
      (strs = """["10","0","1"]""", m = 1, n = 1, expected = 2),
    ).foreach { case (strs, m, n, expected) =>
        test(s"findMaxForm($strs, $m, $n) = $expected"):
            assertEquals(findMaxForm(read[Array[String]](strs), m, n), expected)
    }
