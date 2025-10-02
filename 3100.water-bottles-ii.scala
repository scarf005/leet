package leet.`3100`

import annotation.tailrec

object Solution:
    def maxBottlesDrunk(numBottles: Int, numExchange: Int): Int =
        @tailrec def go(bottles: Int, exchange: Int, total: Int): Int =
            if bottles < exchange then total
            else go(bottles - (exchange - 1), exchange + 1, total + 1)

        go(numBottles, numExchange, numBottles)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = (numBottles = 13, numExchange = 6), expected = 15),
      (input = (numBottles = 10, numExchange = 3), expected = 13),
    ).foreach { case (input, expected) =>
        test(s"maxBottlesDrunk(${input}) = ${expected}"):
            assertEquals(maxBottlesDrunk(input.numBottles, input.numExchange), expected)
    }
