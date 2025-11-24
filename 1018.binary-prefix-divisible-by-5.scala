package leet.`1018`

import scala.collection.mutable.ArrayBuilder

object Solution:
    def prefixesDivBy5(nums: Array[Int]): List[Boolean] =
        val bs = ArrayBuilder.ofBoolean()
        nums.foldLeft(0): (prefix, n) =>
            val next = (prefix << 1 | n) % 5
            bs += (next == 0)
            next
        bs.result.toList

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[0,1,1]", expected = List(true, false, false)),
      (input = "[1,1,1]", expected = List(false, false, false)),
    ).foreach { case (input, expected) =>
        test(s"prefixesDivBy5($input) = $expected"):
            assertEquals(prefixesDivBy5(read[Array[Int]](input)), expected)
    }
