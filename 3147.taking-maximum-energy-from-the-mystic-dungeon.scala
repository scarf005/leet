package leet.`3147`

object Solution:
    def maximumEnergy(energy: Array[Int], k: Int): Int =
        var res = energy.view.takeRight(k).max
        for i <- energy.size - k - 1 to 0 by -1 do
            energy(i) += energy(i + k)
            res = res max energy(i)
        res

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[5,2,-10,-5,1]", k = 3, expected = 3),
      (input = "[-2,-3,-1]", k = 2, expected = -1),
    ).foreach { case (input, k, expected) =>
        test(s"maximumEnergy($input, $k) = $expected"):
            assertEquals(maximumEnergy(read[Array[Int]](input), k), expected)
    }
