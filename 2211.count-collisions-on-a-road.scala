package leet.`2211`

object Solution:
    def countCollisions(directions: String): Int =
        val dirs = "^L+|R+$".r.replaceAllIn(directions, "")
        dirs.size - dirs.count(_ == 'S')

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (directions = "R", output = 0),
      (directions = "RLRSLL", output = 5),
      (directions = "LLRR", output = 0),
    ).foreach { case (input, expected) =>
        test(s"countCollisions($input) = $expected"):
            assertEquals(countCollisions(input), expected)
    }
