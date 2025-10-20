package leet.`2011`

object Solution:
    def finalValueAfterOperations(operations: Array[String]): Int =
        2 * operations.count(_(1) == '+') - operations.size

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (operations = """["--X","X++","X++"]""", expected = 1),
      (operations = """["++X","++X","X++"]""", expected = 3),
    ).foreach { case (operations, expected) =>
        test(s"finalValueAfterOperations($operations) = $expected"):
            assertEquals(finalValueAfterOperations(read[Array[String]](operations)), expected)
    }
