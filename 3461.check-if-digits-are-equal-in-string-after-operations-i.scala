package leet.`3461`

extension (xs: IndexedSeq[Int])
    inline def step =
        xs.sliding(2).map { case Seq(a, b) => (a + b) % 10 }.toIndexedSeq

object Solution:
    def hasSameDigits(s: String): Boolean =
        val nums = s.map(_.asDigit)
        val Seq(a, b) = Iterator.iterate(nums)(_.step).dropWhile(_.length > 2).next
        a == b

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "3902", expected = true),
      (input = "34789", expected = false),
    ).foreach { case (input, expected) =>
        test(s"hasSameDigits($input) = $expected"):
            assertEquals(hasSameDigits(input), expected)
    }
