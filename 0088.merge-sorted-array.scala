package leet.`0088`

import scala.annotation.tailrec

object Solution:
    def merge(xs: Array[Int], m: Int, ys: Array[Int], n: Int): Unit =
        @tailrec def go(i: Int = m - 1, j: Int = n - 1, k: Int = m + n - 1): Unit =
            // println(s"${xs.mkString(", ")} | ${ys.mkString(", ")}")
            // println(s"i=$i, j=$j, k=$k")
            (xs.lift(i), ys.lift(j)) match
                case (Some(x), Some(y)) if x > y => xs(k) = x; go(i - 1, j, k - 1)
                case (_, Some(y))                => xs(k) = y; go(i, j - 1, k - 1)
                case _                           =>
        go()
