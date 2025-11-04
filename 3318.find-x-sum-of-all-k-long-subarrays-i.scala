package leet.`3318`
import scala.collection.mutable.PriorityQueue

object Solution:
    def findXSum(nums: Array[Int], k: Int, x: Int): Array[Int] =
        inline def getXSum(nums: Array[Int]): Int =
            val freqs = nums.groupMapReduce(identity)(_ => 1)(_ + _)
            freqs.toArray.sortInPlaceBy(n => (-n._2, -n._1)).iterator.take(x).map(_ * _).sum

        nums.sliding(k).map(getXSum).toArray

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = "[1,1,2,2,3,4,2,3]", k = 6, x = 2, output = "[6,10,12]"),
      (nums = "[3,8,7,8,7,5]", k = 2, x = 2, output = "[11,15,15,15,12]"),
      (nums = "[1,4,3,5]", k = 4, x = 2, output = "[9]"),
    ).foreach { case (nums, k, x, expected) =>
        test(s"findXSum($nums, $k, $x) = $expected"):
            assertEquals(findXSum(read[Array[Int]](nums), k, x).toSeq, read[Seq[Int]](expected))
    }
