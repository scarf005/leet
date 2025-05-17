package leet.`0075`

object Solution:
    def sortColors(nums: Array[Int]): Unit =
        var `0s` = 0
        var `1s` = 0
        var `2s` = 0
        nums.foreach {
            case 0 => `0s` += 1
            case 1 => `1s` += 1
            case 2 => `2s` += 1
        }
        for i <- 0 until `0s` do nums(i) = 0
        for i <- 0 until `1s` do nums(`0s` + i) = 1
        for i <- 0 until `2s` do nums(`0s` + `1s` + i) = 2
