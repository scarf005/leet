package leet.`1450`

object Solution:
    def busyStudent(starts: Array[Int], ends: Array[Int], at: Int): Int =
        (starts zip ends).count { (start, end) => start to end contains at }
