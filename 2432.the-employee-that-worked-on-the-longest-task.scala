package leet.`2432`

object Solution:
    case class Work(id: Int, duration: Int)

    def hardestWorker(n: Int, logs: Array[Array[Int]]): Int =
        val times = (Array(-1, 0) +: logs)
            .sliding(2)
            .map { case Array(a, b) => Work(b(0), b(1) - a(1)) }

        times.foldLeft((Int.MaxValue, 0)) { case ((minId, maxDuration), Work(id, duration)) =>
            if duration > maxDuration then (id, duration)
            else if duration == maxDuration then (minId min id, maxDuration)
            else (minId, maxDuration)
        }._1
