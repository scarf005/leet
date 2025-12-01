package leet.`2141`

object Solution:
    def maxRunTime(n: Int, batteries: Array[Int]): Long = maxRunTime(n, batteries.map(_.toLong))
    def maxRunTime(n: Int, batteries: Array[Long]): Long =
        var (left, right) = (1L, batteries.sum / n)
        while left < right do
            val mid = right - (right - left) / 2
            val extra = batteries.map { b => b min mid }.sum
            if extra / n >= mid then left = mid else right = mid - 1
        left

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (n = 2, batteries = "[3,3,3]", output = 4L),
      (n = 2, batteries = "[1,1,1,1]", output = 2L),
    ).foreach { case (n, batteries, expected) =>
        test(s"maxRunTime($n, $batteries) = $expected"):
            assertEquals(
              maxRunTime(n, read[Array[Int]](batteries)),
              expected,
            )
    }
