package leet.`3174`

import scala.annotation.tailrec

object Solution:
    def clearDigits(s: String): String =
        val sb = StringBuilder()
        s.foreach { c =>
            if c.isDigit && sb.nonEmpty then sb.deleteCharAt(sb.size - 1) else sb.append(c)
        }
        sb.toString

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    test("cases"):
        assertEquals(clearDigits("abc"), "abc")
        assertEquals(clearDigits("cb34"), "")
