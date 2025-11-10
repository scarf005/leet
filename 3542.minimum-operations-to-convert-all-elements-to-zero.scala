package leet.`3542`

import scala.collection.mutable.Stack

object Solution:
    def minOperations(nums: Array[Int]): Int =
        val stack = Stack.empty[Int]
        var res = 0
        for num <- nums do
            stack.popWhile(top => top > num)
            if stack.headOption.getOrElse(0) < num then
                res += 1
                stack.push(num)
        res

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[0,2]", expected = 1),
      (input = "[3,1,2,1]", expected = 3),
      (input = "[1,2,1,2,1,2]", expected = 4),
    ).foreach { case (input, expected) =>
        test(s"minOperations($input) = $expected"):
            assertEquals(minOperations(read[Array[Int]](input)), expected)
    }
