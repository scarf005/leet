package leet.`1039`

def memoize[I, O](f: (I => O)): I => O =
    val cache = collection.mutable.HashMap.empty[I, O]
    (key) => cache.getOrElseUpdate(key, f(key))

object Solution:
    def minScoreTriangulation(values: Array[Int]): Int =
        inline def score(a: Int, b: Int, c: Int): Int = values(a) * values(b) * values(c)
        lazy val dp: ((Int, Int)) => Int = memoize: (i, j) =>
            (i + 2 - j).sign match
                case 1  => 0
                case 0  => score(i, i + 1, j)
                case -1 =>
                    (i + 1 until j).iterator.map { k => score(i, k, j) + dp(i, k) + dp(k, j) }.min

        dp(0, values.size - 1)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,2,3]", expected = 6),
      (input = "[3,7,4,5]", expected = 144),
      (input = "[1,3,1,4,1,5]", expected = 13),
    ).foreach { case (input, expected) =>
        test(s"minScoreTriangulation(${input}) = ${expected}"):
            assertEquals(minScoreTriangulation(read[Array[Int]](input)), expected)
    }
