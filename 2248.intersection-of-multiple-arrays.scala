package leet.`2248`

object Solution:
    def intersection(nums: Array[Array[Int]]): List[Int] =
        nums.iterator.map(_.toSet).reduce(_ intersect _).toList.sorted
