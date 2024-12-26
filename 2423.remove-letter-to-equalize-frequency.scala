package leet.`2423`

object Solution:
    def equalFrequency(word: String) =
        val freqs = word.groupMapReduce(identity)(_ => 1)(_ + _)
        val vals = freqs.values.toVector
        val keys = vals.toSet.toList.sorted
        println(s"$word -> freqs: $freqs, vals: $vals, keys: $keys")
        keys match
            case List(a) => a == 1 || freqs.keySet.size == 1
            case List(a, b) =>
                vals.count(_ == a) == 1 && (a == 1 || a == b + 1)
                || vals.count(_ == b) == 1 && (b == 1 || b == a + 1)
            case _ => false

@main def main() =
    import leet.`2423`.Solution.equalFrequency

    def test(v: Boolean, expected: Boolean) =
        println(v)
        assert(v == expected)
    test(equalFrequency("abcc"), true)
    test(equalFrequency("aazz"), false)
    test(equalFrequency("bac"), true)
    test(equalFrequency("adbc"), true)
    test(equalFrequency("abbcc"), true)
    test(equalFrequency("cbccca"), false)
    test(equalFrequency("zz"), true)
