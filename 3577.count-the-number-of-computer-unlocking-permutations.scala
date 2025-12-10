package leet.`3577`

object Solution:
    val MOD = 1_000_000_007
    def countPermutations(complexity: Array[Int]): Int =
        if complexity.tail.exists(_ <= complexity.head) then 0
        else (2 until complexity.size).foldLeft(1L) { (a, b) => a * b % MOD }.toInt

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
    ).foreach { case (input, expected) =>
        test(s"countPermutations($input) = $expected"):
            assertEquals(countPermutations(input), expected)
    }
