package leet.`2423`

object Solution:
    def equalFrequency(word: String) =
        val freqs = word.groupMapReduce(identity)(_ => 1)(_ + _)
        val vals = freqs.values.toVector
        vals.toSet.toList.sorted match
            case List(a) => a == 1 || freqs.keySet.size == 1
            case List(a, b) =>
                vals.count(_ == a) == 1 && (a == 1 || a == b + 1)
                || vals.count(_ == b) == 1 && (b == 1 || b == a + 1)
            case _ => false

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    test("cases"):
        assertEquals(equalFrequency("abcc"), true)
        assertEquals(equalFrequency("aazz"), false)
        assertEquals(equalFrequency("bac"), true)
        assertEquals(equalFrequency("adbc"), true)
        assertEquals(equalFrequency("abbcc"), true)
        assertEquals(equalFrequency("cbccca"), false)
        assertEquals(equalFrequency("zz"), true)
