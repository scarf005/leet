package leet.`0166`

import math.Integral.Implicits.infixIntegralOps
import collection.mutable
import annotation.tailrec

object Solution:
    private inline def sign(numerator: Long, denominator: Long): String =
        if numerator < 0 ^ denominator < 0 then "-" else ""

    def fractionToDecimal(numerator: Long, denominator: Long): String =
        if numerator == 0 then "0"
        else sign(numerator, denominator) + impl(numerator.abs, denominator.abs)

    private def impl(numerator: Long, denominator: Long): String =
        val (dividend, remainder) = numerator /% denominator
        if remainder == 0 then dividend.toString
        else
            val sb = new StringBuilder(s"$dividend.")
            val seen = mutable.Map.empty[Long, Int]

            @tailrec def go(remainder: Long, idx: Int): Unit =
                if remainder == 0 then {}
                else if seen.contains(remainder) then
                    sb.insert(seen(remainder), '(')
                    sb.append(')')
                else
                    seen(remainder) = idx
                    val (d, r) = (remainder * 10) /% denominator
                    sb.append(d)
                    go(r, idx + 1)

            go(remainder, sb.length)
            sb.toString

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    List(
      (numerator = 0, denominator = -5, expected = "0"),
      (numerator = 0, denominator = 5, expected = "0"),
      (numerator = 1, denominator = 2, expected = "0.5"),
      (numerator = 2, denominator = 1, expected = "2"),
      (numerator = 4, denominator = 333, expected = "0.(012)"),
    ).foreach { case (numerator, denominator, expected) =>
        test(s"fractionToDecimal(${numerator}, ${denominator}) = ${expected}"):
            assertEquals(fractionToDecimal(numerator, denominator), expected)
    }
