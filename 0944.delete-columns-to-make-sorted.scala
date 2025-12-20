package leet.`0944`

object Solution:
    def minDeletionSize(strs: Array[String]): Int =
        strs.toVector.transpose.count { s => s != s.sorted }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = """["cba","daf","ghi"]""", expected = 1),
      (input = """["a","b"]""", expected = 0),
      (input = """["zyx","wvu","tsr"]""", expected = 3),
    ).foreach { case (input, expected) =>
        test(s"minDeletionSize($input) = $expected"):
            assertEquals(minDeletionSize(read[Array[String]](input)), expected)
    }
