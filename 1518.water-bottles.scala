package leet.`1518`

object Solution:
    def numWaterBottles(bottles: Int, exchange: Int): Int = bottles + (bottles - 1) / (exchange - 1)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    List(
      (input = (bottles = 9, exchange = 3), expected = 13),
      (input = (bottles = 15, exchange = 4), expected = 19),
    ).foreach { case ((bottles, exchange), expected) =>
        test(s"numWaterBottles($bottles, $exchange) = ${expected}"):
            assertEquals(numWaterBottles(bottles, exchange), expected)
    }
