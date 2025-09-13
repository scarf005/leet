package leet.`3541`

import scala.collection.BitSet

extension (inline c: Char)
    inline def a = c - 'a'
    inline def isVowel = vowels(c.a)

val vowels = BitSet("aeiou".map(_.a)*)

extension [A](xs: Iterable[A])
    inline def frequencies = xs.groupMapReduce(identity)(_ => 1)(_ + _)
    inline def mostFrequent = xs.frequencies.values.maxOption.getOrElse(0)

object Solution:
    def maxFreqSum(s: String): Int =
        val (vowels, consonants) = s.partition(_.isVowel)
        vowels.mostFrequent + consonants.mostFrequent

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    List(
      (input = "successes", expected = 6),
      (input = "aeiaeia", expected = 3),
    ).foreach { case (input, expected) =>
        test(s"maxFreqSum($input)"):
            assertEquals(maxFreqSum(input), expected)
    }
