package leet.`0966`

val vowels = "aeiou".map { c => c -> '*' }.toMap.withDefault(identity)
extension (s: String)
    inline def toKey: String = s.toLowerCase.devowel
    inline def devowel: String = s.map(vowels)

extension [A](set: Set[A]) inline def get(key: A) = Option.when(set.contains(key))(key)

object Solution:
    def spellchecker(wordlist: Array[String], queries: Array[String]): Array[String] =
        val exact = wordlist.toSet
        val (caseInsensitive, devoweled) =
            val rev = wordlist.view.reverse
            (rev.map { w => w.toLowerCase -> w }.toMap, rev.map { w => w.toKey -> w }.toMap)

        inline def get(query: String) = exact
            .get(query)
            .orElse(caseInsensitive.get(query.toLowerCase))
            .orElse(devoweled.get(query.toKey))
            .getOrElse("")

        queries.map(get(_)).toArray

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    type Input = Array[String]

    List(
      (
        input = (
          wordlist = """["KiTe","kite","hare","Hare"]""",
          queries = """["kite","Kite","KiTe","Hare","HARE","Hear","hear","keti","keet","keto"]""",
        ),
        expected = """["kite","KiTe","KiTe","Hare","hare","","","KiTe","","KiTe"]""",
      ),
      (
        input = (wordlist = """["yellow"]""", queries = """["YellOw"]"""),
        expected = """["yellow"]""",
      ),
    ).foreach { case ((wordlist, queries), expected) =>
        test(s"spellchecker($wordlist, $queries) = $expected"):
            assertEquals(
              spellchecker(read[Input](wordlist), read[Input](queries)).toList,
              read[Input](expected).toList,
            )
    }
