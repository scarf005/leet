package leet.`3228`

import scala.collection.BitSet

object Solution:
    def maxOperations(s: String): Int =
        val pref = s.map(_.asDigit).scanLeft(0)(_ + _).tail
        s.zipWithIndex.iterator.collect { case ('0', i) => pref(i) }.to(BitSet).sum

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "1001101", expected = 4),
      (input = "00111", expected = 0),
      (input = "110", expected = 2),
    ).foreach { case (input, expected) =>
        test(s"maxOperations($input) = $expected"):
            assertEquals(maxOperations(input), expected)
    }
