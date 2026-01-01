package leet.`0066`

import scala.annotation.tailrec

object Solution:
    def plusOne(digits: Array[Int]): Array[Int] =
        @tailrec def go(carry: Int, index: Int): Array[Int] =
            if index < 0 then if carry == 1 then 1 +: digits else digits
            else
                val sum = digits(index) + carry
                digits(index) = sum % 10
                go(sum / 10, index - 1)

        go(1, digits.size - 1)

import munit.ScalaCheckSuite

class Suite extends ScalaCheckSuite:
    import Solution.*
    import upickle.default.*
    import org.scalacheck.Prop.*
    import org.scalacheck.Gen

    List(
      (input = "[1,2,3]", expected = "[1,2,4]"),
      (input = "[4,3,2,1]", expected = "[4,3,2,2]"),
      (input = "[9]", expected = "[1,0]"),
    ).foreach { case (input, expected) =>
        test(s"plusOne($input)"):
            assertEquals(plusOne(read[Array[Int]](input)).toVector, read[Vector[Int]](expected))
    }

    val validInput: Gen[Array[Int]] = for
        len <- Gen.choose(1, 100)
        digits <- Gen.listOfN(len, Gen.choose(0, 9))
    yield digits.toArray

    property("plusOne") = forAll(validInput) { digits =>
        val originalNumber = BigInt(digits.mkString)
        val result = plusOne(digits.clone())
        val newNumber = BigInt(result.mkString)
        assertEquals(
          newNumber,
          originalNumber + 1,
          s"Input: ${digits.toSeq}, Output: ${result.toSeq}",
        )
    }
