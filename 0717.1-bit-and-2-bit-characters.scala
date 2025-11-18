package leet.`0717`

object Solution:
    def isOneBitCharacter(bits: Array[Int]): Boolean =
        var i = 0
        while i < bits.size - 1 do i += bits(i) + 1
        i == bits.size - 1

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (bits = "[1,0,0]", expected = true),
      (bits = "[1,1,1,0]", expected = false),
    ).foreach { case (input, expected) =>
        test(s"isOneBitCharacter($input) = $expected"):
            assertEquals(isOneBitCharacter(read[Array[Int]](input)), expected)
    }
