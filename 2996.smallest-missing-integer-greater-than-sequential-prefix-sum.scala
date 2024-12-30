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

@main def main() =
    import Solution.*
    println(missingInteger(Array(1, 2, 3, 2, 5)))
    println(missingInteger(Array(4, 5, 6, 7, 8, 8, 9, 4, 3, 2, 7)))
