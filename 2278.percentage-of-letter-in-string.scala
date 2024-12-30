package leet.`2278`

object Solution:
    def percentageLetter(s: String, letter: Char): Int =
        s.toList.count(_ == letter) * 100 / s.length
