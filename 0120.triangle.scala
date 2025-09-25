package leet.`0120`

object Solution:
    def minimumTotal(triangle: List[List[Int]]): Int = triangle.reverse.iterator.reduceLeft {
        (prev, row) => (row lazyZip prev lazyZip prev.tail).map { (x, y, z) => x + (y min z) }
    }.head

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    type Input = List[List[Int]]
    List(
      (input = "[[2],[3,4],[6,5,7],[4,1,8,3]]", expected = 11),
      (input = "[[-10]]", expected = -10),
    ).foreach { case (input, expected) =>
        test(s"minimumTotal(${input}) = ${expected}"):
            assertEquals(minimumTotal(read[Input](input)), expected)
    }
