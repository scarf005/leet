package leet.`2054`

import collection.mutable

case class Value(end: Int, value: Int)
given Ordering[Value] = Ordering.by(-_.end)

case class Event(start: Int, end: Int, value: Int):
    def toValue = Value(end, value)

given Ordering[Event] = Ordering.by(_.start)

object Solution:
    def maxTwoEvents(events: Array[Array[Int]]): Int =
        val evs = events.map(e => Event(e(0), e(1), e(2))).sortInPlace
        val pq = mutable.PriorityQueue.empty[Value]
        var (maxVal, maxSum) = (0, 0)
        for event <- evs do
            while pq.nonEmpty && pq.head.end < event.start do maxVal = maxVal max pq.dequeue().value
            maxSum = maxSum max (maxVal + event.value)
            pq.enqueue(event.toValue)
        maxSum

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (events = "[[1,3,2],[4,5,2],[2,4,3]]", expected = 4),
      (events = "[[1,3,2],[4,5,2],[1,5,5]]", expected = 5),
      (events = "[[1,5,3],[1,5,1],[6,6,5]]", expected = 8),
      (
        events = "[[66,97,90],[98,98,68],[38,49,63],[91,100,42],[92,100,22],[1,77,50],[64,72,97]]",
        expected = 165,
      ),
    ).foreach { case (input, expected) =>
        test(s"maxTwoEvents($input) = $expected"):
            assertEquals(maxTwoEvents(read[Array[Array[Int]]](input)), expected)
    }
