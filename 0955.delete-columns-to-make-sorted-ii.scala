package leet.`0955`

import scala.util.boundary, boundary.break
import scala.collection.mutable

object Solution:
    def minDeletionSize(strs: Array[String]): Int = boundary:
        val n = strs.size
        val pairs = 0 until n - 1
        var delCount = 0
        val sorted = mutable.Set.empty[Int]

        for col <- strs.map(_.toCharArray).transpose do
            if pairs.exists { i => !sorted(i) && col(i) > col(i + 1) } then delCount += 1
            else
                for i <- pairs do if col(i) < col(i + 1) then sorted += i
                if sorted.size == n - 1 then break(delCount)

        delCount

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = """["ca","bb","ac"]""", expected = 1),
      (input = """["xc","yb","za"]""", expected = 0),
      (input = """["zyx","wvu","tsr"]""", expected = 3),
    ).foreach { case (input, expected) =>
        test(s"minDeletionSize($input) = $expected"):
            assertEquals(minDeletionSize(read[Array[String]](input)), expected)
    }
