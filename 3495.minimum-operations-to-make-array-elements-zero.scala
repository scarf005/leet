package leet.`3495`

import scala.collection.Searching.*

extension [A](xs: IndexedSeq[A])
    inline def get(i: Int): Option[A] = Option.when(0 <= i && i < xs.size)(xs(i))
    inline def getOrElse(i: Int, default: A): A = get(i).getOrElse(default)

object Solution:
    inline val step = 14
    val breaks = (0 to step).map(math.pow(4, _).toLong)
    val psums = (0 to step)
        .scanLeft(0L) { (acc, i) => acc + i * (breaks(i) - breaks.getOrElse(i - 1, 0L)) }
        .tail

    def subtotal(n: Long) =
        val i = breaks.search(n) match
            case Found(i)          => i + 1
            case InsertionPoint(i) => i
        psums(i - 1) + i * (n - breaks(i - 1))

    def minOperations(queries: Array[Array[Int]]): Long =
        queries.map { case Array(l, r) => (subtotal(r + 1) - subtotal(l) + 1) / 2 }.sum

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    type Input = Array[Array[Int]]
    List(
      (input = "[[1,2],[2,4]]", expected = 3L),
      (input = "[[2,6]]", expected = 4L),
    ).foreach { case (input, expected) =>
        test(s"minOperations($input) = $expected") {
            assertEquals(minOperations(read[Input](input)), expected)
        }
    }
