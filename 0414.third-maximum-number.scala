package leet.`0414`

extension (a: Int)
    def >(b: Option[Int]) = b.map(a > _).getOrElse(true)
    def <(b: Option[Int]) = b.map(a < _).getOrElse(false)

object Score:
    def empty: (Option[Int], Option[Int], Option[Int]) = (None, None, None)

object Solution:
    def thirdMax(nums: Array[Int]): Int =
        val res = nums.foldLeft(Score.empty) { case (acc @ (first, second, third), n) =>
            if n > first then (Some(n), first, second)
            else if n > second && n < first then (first, Some(n), second)
            else if n > third && n < second then (first, second, Some(n))
            else acc
        }
        res._3.getOrElse(res._1.get)
