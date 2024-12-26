package leet.`3386`

object Solution:
    def buttonWithLongestTime(events: Array[Array[Int]]): Int =
        (Array(0, 0) +: events)
            .sliding(2)
            .collect { case Array(Array(_, t1), Array(id, t2)) => (id, t2 - t1) }
            .maxBy(x => (x._2, -x._1))
            ._1
