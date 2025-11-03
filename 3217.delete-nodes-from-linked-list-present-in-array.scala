package leet.`3217`

import scala.collection.immutable.BitSet
import scala.annotation.tailrec

object Solution:
    def modifiedList(nums: Array[Int], head: ListNode): ListNode =
        val toDelete = nums.to(BitSet)

        var h = head
        while h != null && toDelete(h.x) do h = h.next

        var cursor = h
        while cursor != null do
            if cursor.next != null && toDelete(cursor.next.x) then cursor.next = cursor.next.next
            else cursor = cursor.next
        h

class ListNode(var x: Int = 0, var next: ListNode = null):
    @tailrec final override def equals(that: Any): Boolean = that match
        case that: ListNode =>
            if this.x != that.x then false
            else if this.next == null && that.next == null then true
            else if this.next != null && that.next != null then this.next.equals(that.next)
            else false
        case _ => false

object ListNode:
    def apply(xs: Iterable[Int]): ListNode =
        val dummy = new ListNode(0)
        var cursor = dummy
        for x <- xs do
            cursor.next = new ListNode(x)
            cursor = cursor.next
        dummy.next

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    given Reader[ListNode] = reader[Seq[Int]].map(ListNode.apply)

    List(
      (nums = "[1,2,3]", head = "[1,2,3,4,5]", output = "[4,5]"),
      (nums = "[1]", head = "[1,2,1,2,1,2]", output = "[2,2,2]"),
      (nums = "[5]", head = "[1,2,3,4]", output = "[1,2,3,4]"),
    ).foreach { case (nums, head, expected) =>
        test(s"modifiedList($nums, $head) = $expected") {
            val headList = read[ListNode](head)
            val expectedList = read[ListNode](expected)
            assertEquals(modifiedList(read[Array[Int]](nums), headList), expectedList)
        }
    }
