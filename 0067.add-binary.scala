package leet.`0067`

import scala.annotation.tailrec

object Solution:
    // def addBinary(a: String, b: String): String = { BigInt(a, 2) + BigInt(b, 2) }.toString(2)
    extension (s: String) inline def at(i: Int) = if i >= 0 then s(i) - '0' else 0

    def addBinary(a: String, b: String): String =
        @tailrec def go(i: Int, j: Int, carry: Int, acc: List[Char]): List[Char] =
            if i < 0 && j < 0 && carry == 0 then acc
            else
                val sum = carry + a.at(i) + b.at(j)
                val digit = (sum % 2 + '0').toChar
                go(i - 1, j - 1, carry = sum / 2, acc = digit :: acc)

        go(a.size - 1, b.size - 1, 0, Nil).mkString

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    List(
      (input = ("11", "1"), expected = "100"),
      (input = ("1010", "1011"), expected = "10101"),
      (input = ("111", "10"), expected = "1001"),
      (input = ("10", "111"), expected = "1001"),
    ).foreach { case ((a, b), expected) =>
        test(s"addBinary($a, $b)"):
            assertEquals(addBinary(a, b), expected)
    }
