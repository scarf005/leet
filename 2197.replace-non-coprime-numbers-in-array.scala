package leet.`2197`

import scala.annotation.tailrec

extension (a: Int)
    @tailrec
    infix def gcd(b: Int): Int = if b == 0 then a else b gcd (a % b)
    inline infix def lcm(inline b: Int): Int = a / (a gcd b) * b

object Solution:
    extension (a: Int) inline infix def isNonCoprimeTo(b: Int) = (a gcd b) > 1

    def replaceNonCoprimes(nums: Array[Int]): List[Int] =
        @tailrec def go(stack: List[Int], rest: List[Int]): List[Int] = (stack, rest) match
            case (_, Nil)                                 => stack.reverse
            case (x :: xs, y :: ys) if y isNonCoprimeTo x => go(xs, (x lcm y) :: ys)
            case (stack, y :: ys)                         => go(y :: stack, ys)

        go(Nil, nums.toList)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[6,4,3,2,7,6,2]", expected = "[12,7,6]"),
      (input = "[2,2,1,1,3,3,3]", expected = "[2,1,1,3]"),
      (input = "[31,97561,97561,97561,97561,97561,97561,97561,97561]", expected = "[31,97561]"),
    ).foreach { case (input, expected) =>
        test(s"replaceNonCoprimes($input)"):
            assertEquals(
              replaceNonCoprimes(read[Array[Int]](input)),
              read[List[Int]](expected),
            )
    }
