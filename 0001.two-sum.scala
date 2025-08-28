package leet.`0001`

import scala.collection.mutable
import scala.util.boundary, boundary.break

object Solution:
    def twoSum(nums: Array[Int], target: Int): Array[Int] = boundary {
        val cache = mutable.Map.empty[Int, Int]
        for ((n, i) <- nums.zipWithIndex) do
            cache.get(target - n) match
                case Some(j) => break(Array(j, i))
                case None    => cache(n) = i
        Array()
    }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    test("cases"):
        assertEquals(twoSum(Array(2, 7, 11, 15), 9).toSeq, Seq(0, 1))
        assertEquals(twoSum(Array(3, 2, 4), 6).toSeq, Seq(1, 2))
        assertEquals(twoSum(Array(3, 3), 6).toSeq, Seq(0, 1))
