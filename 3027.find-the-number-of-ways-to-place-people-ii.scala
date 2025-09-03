package leet.`3027`

object Solution:
    extension (inline b: Boolean) inline def toInt = if b then 1 else 0

    case class Point(y: Int, x: Int)
    given Ordering[Point] = Ordering.by(p => (p.x, -p.y))

    def numberOfPairs(points: Array[Array[Int]]): Int =
        val as = points.view.map { case Array(y, x) => Point(y, x) }.sorted.toVector

        val res =
            for (⇱ @ _, i) <- as.iterator.zipWithIndex
            yield as.view.slice(i + 1, as.size).foldLeft((0, Int.MinValue)) {
                case ((count, maxY), ⇲ @ _) if ⇱.y >= ⇲.y =>
                    (count + (⇲.y > maxY).toInt, maxY max ⇲.y)
                case (acc, _) => acc
            }

        res.map(_._1).sum

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    type Input = Array[Array[Int]]
    List(
      (input = "[[1,1],[2,2],[3,3]]", expected = 0),
      (input = "[[6,2],[4,4],[2,6]]", expected = 2),
      (input = "[[3,1],[1,3],[1,1]]", expected = 2),
      (
        input =
            "[[737151554,-145784279],[684525245,-625780200],[31494705,-669876763],[191059839,-178741274],[333806265,-9781464],[671320405,-932976030],[88043963,-892763055],[392705983,-477496093],[285005148,-56520817],[-6021964,-155060028]]",
        expected = 11,
      ),
    ).foreach { case (input, expected) =>
        test(s"numberOfPairs($input) = $expected") {
            assertEquals(numberOfPairs(read[Input](input)), expected)
        }
    }
