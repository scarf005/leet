package leet.`3379`

object Solution:
    def constructTransformedArray(nums: Array[Int]): Array[Int] =
        val s = nums.size
        nums.indices.toArray.map { i => nums(((i + nums(i)) % s + s) % s) }

@main def main() =
    import Solution.*
    println(constructTransformedArray(Array(3, -2, 1, 1)).toVector)
    println(constructTransformedArray(Array(-1, 4, -1)).toVector)
