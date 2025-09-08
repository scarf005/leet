package leet.`1317`

object Solution:
    extension (inline n: Int) inline def hasZeros = n.toString.exists(_ == '0')

    def getNoZeroIntegers(n: Int): Array[Int] =
        (1 to n).iterator
            .map { a => Array(a, n - a) }
            .find { case Array(a, b) => !a.hasZeros && !b.hasZeros }
            .get

import munit.ScalaCheckSuite

class Suite extends ScalaCheckSuite:
    import Solution.*
    import org.scalacheck.Prop.*
    import org.scalacheck.Gen

    val validN: Gen[Int] = Gen.choose(2, math.pow(10, 4).toInt)

    property("getNoZeroIntegers"):
        forAll(validN) { n =>
            val res = getNoZeroIntegers(n)
            assertEquals(res.sum, n, res.toSeq.toString)
            assert(!res.exists(_.hasZeros), res.toSeq.toString)
        }
