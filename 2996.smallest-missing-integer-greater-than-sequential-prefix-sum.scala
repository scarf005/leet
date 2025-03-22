package leet.`2996`

import scala.annotation.tailrec

object Solution:
    def missingInteger(nums: Array[Int]): Int =
        @tailrec def sequential(i: Int): Int = i match
            case _ if i == nums.length           => i
            case 0                               => sequential(1)
            case n if nums(n - 1) + 1 == nums(n) => sequential(n + 1)
            case _                               => i

        val set = nums.toSet
        val sum = nums.take(sequential(0)).sum
        Iterator.from(sum).find(!set(_)).get

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    test("cases"):
        assertEquals(missingInteger(Array(1, 2, 3, 2, 5)), 6)
        assertEquals(missingInteger(Array(3, 4, 5, 1, 12, 14, 13)), 15)
