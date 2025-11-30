import scala.collection.mutable
import scala.util.boundary, boundary.break

object Solution:
    def minSubarray(nums: Array[Int], p: Int): Int = boundary:
        extension (inline a: Int) inline def +%(inline b: Int) = (a + b) % p

        val n = nums.size
        val totalSum = nums.reduce(_ +% _)
        val target = totalSum % p
        if target == 0 then break(0)

        val mods = mutable.Map(0 -> -1)
        var sum = 0
        var minLen = n
        for (n, i) <- nums.zipWithIndex
        do
            sum = sum +% n
            val needed = (sum - target + p) % p
            mods.get(needed).foreach { rem => minLen = minLen min (i - rem) }
            mods(sum) = i
        if minLen == n then -1 else minLen

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = "[3,1,4,2]", p = 6, output = 1),
      (nums = "[6,3,5,2]", p = 9, output = 2),
      (nums = "[1,2,3]", p = 3, output = 0),
      (nums = "[8,32,31,18,34,20,21,13,1,27,23,22,11,15,30,4,2]", p = 148, output = 7),
    ).foreach { case (nums, p, expected) =>
        test(s"minSubarray(${nums}, ${p}) = ${expected}"):
            assertEquals(
              minSubarray(read[Array[Int]](nums), p),
              expected,
            )
    }
