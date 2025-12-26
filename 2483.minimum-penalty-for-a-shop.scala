package leet.`2483`
import annotation.tailrec

extension (inline b: Boolean) inline def toInt = if b then 1 else 0

object Solution:
    def bestClosingTime(customers: String): Int =
        val n = customers.size
        val closePenalty = customers.count(_ == 'Y')

        @tailrec
        def go(i: Int, curPen: Int, minPen: Int, bestH: Int): Int =
            if i == n then bestH
            else
                val nextPen = curPen + (1 - 2 * (customers(i) == 'Y').toInt)
                go(i + 1, nextPen, nextPen min minPen, if nextPen < minPen then i + 1 else bestH)

        go(0, closePenalty, closePenalty, 0)

    def bestClosingTimeFP(customers: String): Int = customers
        .scanLeft(0) { (pen, c) => pen + (if c == 'Y' then -1 else 1) }
        .zipWithIndex
        .minBy(_._1)
        ._2

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (customers = "YYNY", expected = 2),
      (customers = "NNNNN", expected = 0),
      (customers = "YYYY", expected = 4),
    ).foreach { case (customers, expected) =>
        test(s"bestClosingTime($customers) = $expected"):
            assertEquals(bestClosingTime(customers), expected)
            assertEquals(bestClosingTimeFP(customers), expected)
    }
