package leet.`3516`

object Solution:
    def findClosest(x: Int, y: Int, z: Int): Int =
        math.abs(z - x) `compareTo` math.abs(z - y) match
            case -1 => 1
            case 0  => 0
            case 1  => 2

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = (2, 7, 4), expected = 1),
      (input = (2, 5, 6), expected = 2),
      (input = (1, 5, 3), expected = 0),
    ).foreach { case ((x, y, z), expected) =>
        test(s"findClosest($x, $y, $z) = $expected") {
            assertEquals(findClosest(x, y, z), expected)
        }
    }
