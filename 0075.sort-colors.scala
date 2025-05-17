package leet.`0075`

import scala.annotation.tailrec

object Solution:
    def sortColors(nums: Array[Int]): Unit =
        def swap(i: Int, j: Int) =
            val temp = nums(i)
            nums(i) = nums(j)
            nums(j) = temp

        @tailrec
        def go(zero: Int, one: Int, two: Int): Unit = if one <= two then
            nums(one) match
                case 0 => swap(zero, one); go(zero + 1, one + 1, two)
                case 1 => go(zero, one + 1, two)
                case 2 => swap(one, two); go(zero, one, two - 1)

        go(0, 0, nums.size - 1)

import munit.FunSuite

class Suite extends FunSuite:
    test("sortColors should sort [2,0,2,1,1,0] to [0,0,1,1,2,2]"):
        val nums = Array(2, 0, 2, 1, 1, 0)
        Solution.sortColors(nums)
        assertEquals(nums.toSeq, Seq(0, 0, 1, 1, 2, 2))

    test("sortColors should sort [2,0,1] to [0,1,2]"):
        val nums = Array(2, 0, 1)
        Solution.sortColors(nums)
        assertEquals(nums.toSeq, Seq(0, 1, 2))

    test("sortColors should handle an already sorted array [0,1,2]"):
        val nums = Array(0, 1, 2)
        Solution.sortColors(nums)
        assertEquals(nums.toSeq, Seq(0, 1, 2))

    test("sortColors should handle an array with all elements the same [1,1,1]"):
        val nums = Array(1, 1, 1)
        Solution.sortColors(nums)
        assertEquals(nums.toSeq, Seq(1, 1, 1))

    test("sortColors should handle an empty array"):
        val nums = Array[Int]()
        Solution.sortColors(nums)
        assertEquals(nums.toSeq, Seq())
