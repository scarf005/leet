package leet.`3028`

object Solution:
    extension (b: Boolean) inline def toInt: Int = if b then 1 else 0

    def returnToBoundaryCount(nums: Array[Int]): Int =
        nums.foldLeft((0, 0)) { case ((prev, count), num) =>
            val next = prev + num
            (next, count + (next == 0).toInt)
        }._2
