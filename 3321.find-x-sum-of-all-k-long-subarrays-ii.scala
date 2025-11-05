package leet.`3321`

import scala.collection.mutable.{Map, SortedSet}
import scala.math.Ordered.orderingToOrdered

case class Pair(freq: Int, num: Int):
    val xSum = freq.toLong * num

given Ordering[Pair] = Ordering.by(p => (p.freq, p.num))

class Counter(val x: Int):
    val large = SortedSet.empty[Pair]
    val small = SortedSet.empty[Pair]
    val freqs = Map.empty[Int, Int]
    private var _res: Long = 0L
    def res = _res

    def +=(num: Int) =
        val freq = freqs.getOrElse(num, 0)
        if freq > 0 then internalRemove(Pair(freq, num))
        freqs(num) = freq + 1
        internalInsert(Pair(freq + 1, num))

    def -=(num: Int) =
        val freq = freqs(num)
        internalRemove(Pair(freq, num))
        freqs(num) -= 1
        if freqs(num) > 0 then internalInsert(Pair(freq - 1, num))
        else freqs.remove(num)

    private def internalInsert(p: Pair): Unit =
        if large.size < x || p > large.head then
            large += p
            _res += p.xSum
            if large.size > x then
                val toRemove = large.head
                large -= toRemove
                _res -= toRemove.xSum
                small += toRemove
        else small += p

    private def internalRemove(p: Pair): Unit =
        if large(p) then
            large -= p
            _res -= p.xSum
            small.lastOption.foreach: toAdd =>
                small -= toAdd
                large += toAdd
                _res += toAdd.xSum
        else small -= p

object Solution:
    def findXSum(nums: Array[Int], k: Int, x: Int): Array[Long] =
        val counter = Counter(x)
        val result = Array.ofDim[Long](nums.length - k + 1)

        for i <- nums.indices do
            counter += nums(i)
            if i >= k then counter -= nums(i - k)
            if i >= k - 1 then result(i - k + 1) = counter.res
        result

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (nums = "[1,1,2,2,3,4,2,3]", k = 6, x = 2, output = "[6,10,12]"),
      (nums = "[3,8,7,8,7,5]", k = 2, x = 2, output = "[11,15,15,15,12]"),
      (nums = "[1,4,3,5]", k = 4, x = 2, output = "[9]"),
      (nums = "[1000000000,1000000000,1000000000,1000000000,1000000000,1000000000]", k = 6, x = 1, output = "[6000000000]"),
    ).foreach { case (nums, k, x, expected) =>
        test(s"findXSum($nums, $k, $x) = $expected"):
            assertEquals(findXSum(read[Array[Int]](nums), k, x).toSeq, read[Seq[Long]](expected))
    }
