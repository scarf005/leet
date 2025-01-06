package leet.`1560`

object Solution:
    def mostVisited(n: Int, rounds: Array[Int]): List[Int] =
        val sectors = (1 to n).toSet

        def visited(begin: Int, end: Int): Set[Int] =
            if begin == end then sectors - begin
            else if begin < end then (begin + 1 to end).toSet
            else sectors -- (end + 1 to begin).toSet

        val runs = (rounds
            .sliding(2)
            .collect { case Array(a, b) => visited(a, b).toSeq }
            .flatten
            .toSeq ++ Seq(rounds(0)))
            .groupMapReduce(identity)(_ => 1)(_ + _)

        runs.filter(_._2 == runs.values.max).keys.toList.sorted

@main def main() =
    import Solution.*
    println(mostVisited(4, Array(1, 3, 1, 2)))
    println(mostVisited(2, Array(2, 1, 2, 1, 2, 1, 2, 1, 2)))
