package leet.`3623`

inline val MOD = 1_000_000_007

extension (inline a: Long)
    inline def +%(inline b: Long): Long = (a + b) % MOD
    inline def *%(inline b: Long): Long = a * b % MOD

object Solution:
    def countTrapezoids(points: Array[Array[Int]]): Int = points
        .groupMapReduce(_(1))(_ => 1)(_ + _)
        .values
        .map { p => p.toLong * (p - 1) / 2 }
        .foldLeft((0L, 0L)) { case ((res, sum), edge) => (res +% edge *% sum, sum +% edge) }
        ._1
        .toInt

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (points = "[[1,0],[2,0],[3,0],[2,2],[3,2]]", output = 3),
      (points = "[[0,0],[1,0],[0,1],[2,1]]", output = 1),
      (points = "[[-73,-72],[-1,-56],[-92,30],[-57,-89],[-19,-89],[-35,30],[64,-72]]", output = 3),
    ).foreach { case (points, expected) =>
        test(s"countTrapezoids($points) = $expected"):
            assertEquals(
              countTrapezoids(read[Array[Array[Int]]](points)),
              expected,
            )
    }
