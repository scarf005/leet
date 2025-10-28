package leet.`2125`

object Solution:
    case class Cur(total: Int, prev: Int)
    def numberOfBeams(bank: Array[String]): Int = bank.iterator
        .map(_.count(_ == '1'))
        .foldLeft(Cur(total = 0, prev = 0)):
            case (Cur(total, 0), count)    => Cur(total, count)
            case (Cur(total, prev), 0)     => Cur(total, prev)
            case (Cur(total, prev), count) => Cur(total + prev * count, count)
        .total

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = """["011001","000000","010100","001000"]""", expected = 8),
      (input = """["000","111","000"]""", expected = 0),
      (input = """["1","1"]""", expected = 1),
    ).foreach { case (input, expected) =>
        test(s"numberOfBeams($input) = $expected"):
            assertEquals(numberOfBeams(read[Array[String]](input)), expected)
    }
