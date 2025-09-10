package leet.`1733`

import collection.immutable.BitSet

type Input = Array[Array[Int]]

object Solution:
    def minimumTeachings(n: Int, ls: Input, fs: Input): Int =
        val know = ls.iterator.map(lang => BitSet(lang*)).toVector
        val need = fs.iterator
            .map { case Array(a1, b1) => (a1 - 1, b1 - 1) }
            .filter { (a, b) => (know(a) intersect know(b)).isEmpty }
            .flatMap(Iterator(_, _))
            .to(BitSet)

        println(s"need = $need, know = $know")

        (1 to n).iterator.map { lang => need.count(i => !know(i)(lang)) }.min

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (
        input = (n = 2, ls = "[[1],[2],[1,2]]", fs = "[[1,2],[1,3],[2,3]]"),
        expected = 1,
      ),
      (
        input = (n = 3, ls = "[[2],[1,3],[1,2],[3]]", fs = "[[1,4],[1,2],[3,4],[2,3]]"),
        expected = 2,
      ),
    ).foreach { case ((n, ls, fs), expected) =>
        test(s"minimumTeachings($n, $ls, $fs) = $expected") {
            assertEquals(minimumTeachings(n, read[Input](ls), read[Input](fs)), expected)
        }
    }
