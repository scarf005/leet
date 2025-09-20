package leet.`3335`

object Solution:
    inline val MOD = 1_000_000_007
    extension (inline a: Int) inline def +%(inline b: Int): Int = (a + b) % MOD

    val as: Vector[Int] =
        lazy val xs: LazyList[Int] = LazyList.fill(26)(1) #::: (xs zip xs.drop(1)).map(_ +% _)
        xs.take(100100).toVector

    def lengthAfterTransformations(s: String, t: Int): Int =
        s.iterator.map(_ - 'a' + t).map(as).foldLeft(0)(_ +% _)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    List(
      (s = "abcyy", t = 2, expected = 7),
      (s = "azbk", t = 1, expected = 5),
      (s = "jqktcurgdvlibczdsvnsg", t = 7517, expected = 79033769),
    ).foreach { case (s, t, expected) =>
        test(s"lengthAfterTransformations($s, $t)"):
            assertEquals(lengthAfterTransformations(s, t), expected)
    }
