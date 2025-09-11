package leet.`2785`

import scala.collection.BitSet

object Solution:
    val vowels = BitSet("aeiouAEIOU".map(_.a)*)
    extension (inline c: Char)
        inline def a = c - 'A'
        inline def isVowel = vowels(c.a)

    def sortVowels(s: String): String =
        val vs = s.filter(_.isVowel).sorted.iterator

        s.map { c => if c.isVowel then vs.next() else c }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    List(
      (input = "lEetcOde", expected = "lEOtcede"),
      (input = "lYmpH", expected = "lYmpH"),
    ).foreach { case (input, expected) =>
        test(s"sortVowels($input) = $expected"):
            assertEquals(sortVowels(input), expected)
    }
