package leet.`1390`

import annotation.tailrec

object Solution:
    def sumFourDivisors(nums: Array[Int]): Int =
        nums.iterator.map(getSumIfFourDivisors).sum

    private def getSumIfFourDivisors(n: Int): Int =
        val root = math.sqrt(n).toInt
        if n < 6 || root * root == n then 0
        else
            @tailrec def go(d: Int, count: Int, curSum: Int): Int =
                if count > 4 then 0
                else if d > root then if count == 4 then curSum else 0 // finished checking
                else if n % d == 0
                then go(d + 1, count + 2, curSum + d + (n / d)) // found divisor pair
                else go(d + 1, count, curSum) // not a divisor

            go(1, 0, 0)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[21,4,7]", expected = 32),
      (input = "[21,21]", expected = 64),
      (input = "[1,2,3,4,5]", expected = 0),
    ).foreach { case (input, expected) =>
        test(s"sumFourDivisors($input) = $expected"):
            assertEquals(sumFourDivisors(read[Array[Int]](input)), expected)
    }
