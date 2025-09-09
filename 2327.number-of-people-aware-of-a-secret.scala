package leet.`2327`

object Solution:
    inline val MOD = 1_000_000_007

    extension (xs: Array[Long])
        inline def at(inline i: Int) = ((if (i < 0) 0L else xs(i)) + MOD) % MOD

    def peopleAwareOfSecret(n: Int, delay: Int, forget: Int): Int =
        val cache = Array.fill(n + 1)(0L)
        cache(1) = 1

        var share = 0L
        for day <- 2 to n do
            share += cache.at(day - delay) - cache.at(day - forget)
            cache(day) = share

        cache.view.drop(n - forget + 1).reduce((a, b) => (a + b) % MOD).toInt

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    List(
      (input = (n = 6, delay = 2, forget = 4), expected = 5),
      (input = (n = 4, delay = 1, forget = 3), expected = 6),
      (input = (n = 684, delay = 18, forget = 496), expected = 653668527),
    ).foreach { case ((n, delay, forget), expected) =>
        test(s"peopleAwareOfSecret($n, $delay, $forget) = $expected"):
            assertEquals(peopleAwareOfSecret(n, delay, forget), expected)
    }
