package leet.`2255`

object Solution:
    def countPrefixes(words: Array[String], s: String): Int = words.count(_.startsWith(s))
