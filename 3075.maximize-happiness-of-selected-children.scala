package leet.`3075`

object Solution:
    def maximumHappinessSum(happiness: Array[Int], k: Int): Long =
        val n = happiness.sortInPlace.size
        (0 until k).foldLeft(0L) { (acc, i) => acc + (0 max (happiness(n - 1 - i) - i)) }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (happiness = "[1,2,3]", k = 2, expected = 4L),
      (happiness = "[1,1,1,1]", k = 2, expected = 1L),
      (happiness = "[2,3,4,5]", k = 1, expected = 5L),
      (happiness = "[12,1,42]", k = 3, expected = 53L),
    ).foreach { case (happinessStr, k, expected) =>
        test(s"maximumHappinessSum($happinessStr, $k) = $expected"):
            assertEquals(maximumHappinessSum(read[Array[Int]](happinessStr), k), expected)
    }
