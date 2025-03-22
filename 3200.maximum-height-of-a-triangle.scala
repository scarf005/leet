package leet.`3200`

object Solution:
    inline def oddRow(n: Int) = n * n
    inline def evenRow(n: Int) = n * (n + 1)

    def maxHeightOfTriangle(red: Int, blue: Int) = Iterator
        .from(1)
        .find { n =>
            val odd = oddRow((n + n % 2) / 2)
            val even = evenRow((n - n % 2) / 2)

            val blueOnTop = odd <= blue && even <= red
            val redOnTop = odd <= red && even <= blue
            !(blueOnTop || redOnTop)
        }
        .get - 1

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    test("cases"):
        assertEquals(maxHeightOfTriangle(2, 4), 3)
        assertEquals(maxHeightOfTriangle(2, 1), 2)
        assertEquals(maxHeightOfTriangle(1, 1), 1)
        assertEquals(maxHeightOfTriangle(10, 1), 2)
