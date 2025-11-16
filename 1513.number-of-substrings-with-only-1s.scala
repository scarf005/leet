package leet.`1513`

extension [A](xs: IterableOnce[A])
    def consecutives(elem: A): Iterator[Int] = new Iterator[Int]:
        private val it = xs.iterator.buffered

        def hasNext: Boolean =
            while it.hasNext && it.head != elem do it.next()
            it.hasNext

        def next(): Int =
            if (!hasNext) throw new NoSuchElementException("next on empty iterator")
            var count = 0
            while it.hasNext && it.head == elem do
                count += 1
                it.next()

            count

object Solution:
    inline val MOD = 1_000_000_007
    inline def count(inline n: Int): Int = ((n.toLong * (n + 1) / 2) % MOD).toInt
    def numSub(s: String): Int = s.consecutives('1').foldLeft(0) { (a, b) => (a + count(b)) % MOD }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "0110111", expected = 9),
      (input = "101", expected = 2),
      (input = "111111", expected = 21),
    ).foreach { case (input, expected) =>
        test(s"numSub($input) = $expected"):
            assertEquals(numSub(input), expected)
    }
