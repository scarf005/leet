package leet.`0960`

object Solution:
    def minDeletionSize(strs: Array[String]): Int =
        val w = strs.head.size
        val dp = Array.fill(w)(1)
        for
            i <- w - 2 to 0 by -1
            j <- i + 1 until w
            if strs.forall { row => row(i) <= row(j) }
        do dp(i) = dp(i) max (dp(j) + 1)
        w - dp.max

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = """["babca","bbazb"]""", expected = 3),
      (input = """["edcba"]""", expected = 4),
      (input = """["ghi","def","abc"]""", expected = 0),
    ).foreach { case (input, expected) =>
        test(s"minDeletionSize($input) = $expected"):
            assertEquals(minDeletionSize(read[Array[String]](input)), expected)
    }
