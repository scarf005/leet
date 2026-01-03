package leet.`1411`

inline val MOD = 1_000_000_007
extension (n: Long) inline def +%(m: Long): Long = (n + m) % MOD
extension (tup: (Long, Long)) inline def sum: Int = (tup._1 +% tup._2).toInt

object Solution:
    def numOfWays(n: Int): Int = (2 to n)
        .foldLeft((6L, 6L)) { case ((a, b), _) => ((2 * a) +% (2 * b), (2 * a) +% (3 * b)) }
        .sum

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = 1, expected = 12),
      (input = 5000, expected = 30228214),
    ).foreach { case (input, expected) =>
        test(s"numOfWays($input) = $expected"):
            assertEquals(numOfWays(input), expected)
    }
