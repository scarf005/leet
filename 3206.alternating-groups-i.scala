package leet.`3206`

object Solution:
    def numberOfAlternatingGroups(colors: Array[Int]): Int =
        def at(i: Int): Int = colors((i + colors.size) % colors.size)
        colors.zipWithIndex.count((c, i) => c != at(i - 1) && c != at(i + 1))
