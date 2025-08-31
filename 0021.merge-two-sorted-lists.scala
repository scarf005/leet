package leet.`0021`

object Solution:
    def mergeTwoLists(as: ListNode, bs: ListNode): ListNode = (as, bs) match
        case (null, bs) => bs
        case (as, null) => as
        case (a, b)     =>
            if a.x > b.x
            then ListNode(b.x, mergeTwoLists(a, b.next))
            else ListNode(a.x, mergeTwoLists(a.next, b))

import munit.FunSuite

class ListNode(_x: Int = 0, _next: ListNode = null):
    var next: ListNode = _next
    var x: Int = _x

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    def fromList(xs: Seq[Int]): ListNode =
        if xs.isEmpty then null
        else ListNode(xs.head, fromList(xs.tail))

    def toList(node: ListNode): List[Int] =
        if node == null then Nil
        else node.x :: toList(node.next)

    type Input = Seq[Int]
    inline def parse(input: String): Input = read[Input](input)
    List(
      (as = "[1,2,4]", bs = "[1,3,4]", expected = "[1,1,2,3,4,4]"),
      (as = "[]", bs = "[]", expected = "[]"),
      (as = "[]", bs = "[0]", expected = "[0]"),
    ).foreach { case (as, bs, expected) =>
        test(s"mergeTwoLists($as, $bs) = $expected"):
            assertEquals(
              toList(mergeTwoLists(fromList(parse(as)), fromList(parse(bs)))),
              parse(expected),
            )
    }
