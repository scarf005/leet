package leet.`1304`

object Solution:
    def sumZero(n: Int): Array[Int] = (0 until n).iterator.map(i => i * 2 - n + 1).toArray

import munit.ScalaCheckSuite

class Suite extends ScalaCheckSuite:
    import Solution.*
    import org.scalacheck.Gen
    import org.scalacheck.Prop.*

    val validN = Gen.choose(1, 1000)

    property("sumZero") = forAll(validN) { n =>
        val res = sumZero(n)
        assertEquals(res.sum, 0, res.toSeq)
        assertEquals(res.distinct.size, n, res.toSeq)
    }
