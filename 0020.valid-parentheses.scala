package leet.`0020`

import scala.collection.mutable.Stack
import scala.util.boundary, boundary.break

object Solution:
    val parens = Map('(' -> ')', '{' -> '}', '[' -> ']')

    extension [A](stack: Stack[A])
        inline def popOption(): Option[A] = if stack.isEmpty then None else Some(stack.pop())

    def isValid(s: String): Boolean = boundary:
        if s.size % 2 != 0 then break(false)

        val stack = Stack[Char]()
        for c <- s do
            parens.get(c) match
                case Some(closing) => stack.push(closing)
                case None          => if Some(c) != stack.popOption() then break(false)

        stack.isEmpty

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    List(
      (s = "()", expected = true),
      (s = "()[]{}", expected = true),
      (s = "(]", expected = false),
      (s = "([])", expected = true),
      (s = "([)]", expected = false),
      (s = "){", expected = false),
    ).foreach { case (s, expected) =>
        test(s"isValid($s)"):
            assertEquals(isValid(s), expected)
    }
