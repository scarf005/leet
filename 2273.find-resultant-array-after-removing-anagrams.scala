package leet.`2273`

import scala.annotation.tailrec

extension (a: String) inline infix def isAnagram(b: String): Boolean = a.sorted == b.sorted

object Solution:
    def removeAnagrams(words: Array[String]): List[String] =
        @tailrec def go(as: List[String], bs: List[String]): List[String] = (as, bs) match
            case (Nil, _)                           => bs.reverse
            case (x :: xs, y :: _) if x isAnagram y => go(xs, bs)
            case (x :: xs, _)                       => go(xs, x :: bs)

        go(words.toList, Nil)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = """["abba","baba","bbaa","cd","cd"]""", expected = """["abba","cd"]"""),
      (input = """["a","b","c","d","e"]""", expected = """["a","b","c","d","e"]"""),
    ).foreach { case (input, expected) =>
        test(s"removeAnagrams($input) = $expected"):
            assertEquals(removeAnagrams(read[Array[String]](input)), read[List[String]](expected))
    }
