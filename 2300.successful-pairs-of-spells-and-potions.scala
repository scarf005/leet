package leet.`2300`

import collection.Searching.*
import annotation.tailrec

extension (xs: collection.IndexedSeq[Int])
    inline def bisectLeftBy(elem: Long)(f: Int => Long): SearchResult =
        @tailrec def go(low: Int, high: Int): SearchResult =
            if low >= high then InsertionPoint(low)
            else
                val mid = low + ((high - low) >>> 1)
                if f(xs(mid)) >= elem then go(low, mid) else go(mid + 1, high)
        go(0, xs.length)

object Solution:
    def successfulPairs(ss: Array[Int], ps: Array[Int], success: Long): Array[Int] =
        ps.sortInPlace
        ss.map { spell => ps.size - ps.bisectLeftBy(success)(_.toLong * spell).insertionPoint }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (spells = "[5,1,3]", potions = "[1,2,3,4,5]", success = 7L, expected = "[4,0,3]"),
      (spells = "[3,1,2]", potions = "[8,5,8]", success = 16L, expected = "[2,0,2]"),
      (
        spells =
            "[15,39,38,35,33,25,31,12,40,27,29,16,22,24,7,36,29,34,24,9,11,35,21,3,33,10,9,27,35,17,14,3,35,35,39,23,35,14,31,7]",
        potions =
            "[25,19,30,37,14,30,38,22,38,38,26,33,34,23,40,28,15,29,36,39,39,37,32,38,8,17,39,20,4,39,39,7,30,35,29,23]",
        success = 317L,
        expected =
            "[28,33,33,33,33,33,33,23,34,33,33,29,32,33,0,33,33,33,33,13,22,33,31,0,33,17,13,33,33,30,27,0,33,33,33,33,33,27,33,0]",
      ),
      (spells = "[100000]", potions = "[100000]", success = 10000000000L, expected = "[1]"),
    ).foreach { case (spells, potions, success, expected) =>
        test(s"successfulPairs($spells, $potions, $success) = $expected"):
            assertEquals(
              successfulPairs(read[Array[Int]](spells), read[Array[Int]](potions), success).toSeq,
              read[Seq[Int]](expected),
            )
    }
