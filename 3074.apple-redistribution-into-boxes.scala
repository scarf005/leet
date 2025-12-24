package leet.`3074`

import scala.annotation.tailrec
import scala.collection.View

object Solution:
    def minimumBoxes(apple: Array[Int], capacity: Array[Int]): Int =
        go(apple.sum, capacity.sortInPlaceBy(-_).view)

    @tailrec def go(apples: Int, capacity: View[Int], acc: Int = 0): Int =
        if apples <= 0 then acc else go(apples - capacity.head, capacity.tail, acc + 1)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (apple = "[1,3,2]", capacity = "[4,3,1,5,2]", expected = 2),
      (apple = "[5,5,5]", capacity = "[2,4,2,7]", expected = 4),
    ).foreach { case (appleStr, capacityStr, expected) =>
        test(s"minimumBoxes($appleStr, $capacityStr) = $expected"):
            assertEquals(
              minimumBoxes(read[Array[Int]](appleStr), read[Array[Int]](capacityStr)),
              expected,
            )
    }
