package leet.`1792`

import scala.collection.mutable

object Solution:
    extension [A](xs: Iterable[A])
        inline def averageBy(inline f: A => Double): Double = xs.map(f).sum / xs.size

    extension (inline p: Int) inline infix def avg(inline t: Int) = p.toDouble / t

    case class Class(p: Int, t: Int):
        lazy val add1profit = (this + 1).ratio - ratio
        val ratio = p avg t
        inline def +(students: Int) = Class(p + students, t + students)

    given Ordering[Class] = Ordering.by(_.add1profit)

    def maxAverageRatio(classes: Array[Array[Int]], extraStudents: Int): Double =
        val pq = mutable.PriorityQueue.from(classes.map { case Array(p, t) => Class(p, t) })

        for _ <- 1 to extraStudents do pq.enqueue(pq.dequeue() + 1)
        pq.averageBy(_.ratio)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    type Input = Array[Array[Int]]
    List(
      (classes = "[[1,2],[3,5],[2,2]]", extraStudents = 2, expected = 0.78333),
      (classes = "[[2,4],[3,9],[4,5],[2,10]]", extraStudents = 4, expected = 0.53485),
    ).foreach { case (classes, extraStudents, expected) =>
        test(s"maxAverageRatio($classes, $extraStudents) = $expected"):
            assertEqualsDouble(maxAverageRatio(read[Input](classes), extraStudents), expected, 1e-5)
    }
