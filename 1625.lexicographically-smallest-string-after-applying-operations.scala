package leet.`1625`

import scala.collection.mutable.SortedSet

object Solution:
    def findLexSmallestString(s: String, a: Int, b: Int): String =
        val n = s.length
        val incremented = (0 to 9).map(i => s"$i" -> s"${((i + a) % 10)}").toMap

        extension (s: String)
            inline def addOps = s.zipWithIndex.map { (ch, i) =>
                val s = ch.toString
                if (i % 2 == 1) incremented(s) else s
            }.mkString
            inline def rotateOps = s.drop(n - b) + s.take(n - b)

        val seen = SortedSet.empty[String]
        def dfs(s: String): Unit = if seen.add(s) then
            dfs(s.addOps)
            dfs(s.rotateOps)
        dfs(s)
        seen.min

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = (s = "5525", a = 9, b = 2), expected = "2050"),
      (input = (s = "74", a = 5, b = 1), expected = "24"),
      (input = (s = "0011", a = 4, b = 2), expected = "0011"),
    ).foreach { case (input = (s, a, b), expected = expected) =>
        test(s"""findLexSmallestString("$s", $a, $b) = "$expected""""):
            assertEquals(findLexSmallestString(s, a, b), expected)
    }
