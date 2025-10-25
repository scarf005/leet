package leet.`1716`

object Solution:
    def totalMoney(n: Int): Int =
        val (week, day) = (n / 7, n % 7)
        val fullWeeks = 28 * week + 7 * (week * (week - 1) / 2)
        val extraDays = day * week + (day * (day + 1) / 2)
        fullWeeks + extraDays

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = 4, expected = 10),
      (input = 10, expected = 37),
      (input = 20, expected = 96),
    ).foreach { case (input, expected) =>
        test(s"totalMoney($input) = $expected"):
            assertEquals(totalMoney(input), expected)
    }
