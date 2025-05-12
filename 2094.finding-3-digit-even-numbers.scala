package leet.`2094`

object Solution:
    extension (a: Map[Int, Int])
        inline infix def contains(b: Map[Int, Int]): Boolean =
            b.forall { (k, v) => a.getOrElse(k, 0) >= v }

    extension [A](a: Iterable[A]) inline def freqs = a.groupMapReduce(identity)(_ => 1)(_ + _)

    val candidates = for
        h <- 1 to 9
        t <- 0 to 9
        o <- 0 to 8 by 2
    yield Seq(h, t, o).freqs -> (h * 100 + t * 10 + o)

    def findEvenNumbers(digits: Array[Int]): Array[Int] =
        val freqs = digits.freqs
        candidates.collect { case (c, n) if freqs contains c => n }.toArray
