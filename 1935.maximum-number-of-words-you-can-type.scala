package leet.`1935`

object Solution:
    def canBeTypedWords(text: String, brokenLetters: String): Int =
        val broken = brokenLetters.toSet
        text.split(" ").count(!_.exists(broken))

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    List(
      (text = "hello world", brokenLetters = "ad", expected = 1),
      (text = "leet code", brokenLetters = "lt", expected = 1),
      (text = "leet code", brokenLetters = "e", expected = 0),
    ).foreach { case (text, brokenLetters, expected) =>
        test(s"canBeTypedWords($text, $brokenLetters)"):
            assertEquals(canBeTypedWords(text, brokenLetters), expected)
    }
