package leet.`3379`

object Solution:
    def constructTransformedArray(nums: Array[Int]): Array[Int] =
        val s = nums.size
        nums.indices.toArray.map { i => nums(((i + nums(i)) % s + s) % s) }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*

    test("cases"):
        assertEquals(constructTransformedArray(Array(3, -2, 1, 1)).toVector, Vector(1, 1, 1, 3))
        assertEquals(constructTransformedArray(Array(-1, 4, -1)).toVector, Vector(-1, -1, 4))
