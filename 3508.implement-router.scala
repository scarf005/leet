package leet.`3508`

import collection.Searching.*
import collection.mutable.{Set, SortedSet, Map, ArrayDeque, IndexedSeq}
import scala.annotation.tailrec

extension [A](xs: collection.IndexedSeq[A])
    inline def bisectLeft[B >: A](elem: B)(using ord: Ordering[B]): SearchResult =
        @tailrec def go(low: Int, high: Int): SearchResult =
            if low >= high then InsertionPoint(low)
            else
                val mid = low + ((high - low) >>> 1)
                if ord.lt(xs(mid), elem) then go(mid + 1, high) else go(low, mid)
        go(0, xs.length)

    inline def bisectRight[B >: A](elem: B)(using ord: Ordering[B]): SearchResult =
        @tailrec def go(low: Int, high: Int): SearchResult =
            if low >= high then InsertionPoint(low)
            else
                val mid = low + ((high - low) >>> 1)
                if ord.lteq(xs(mid), elem) then go(mid + 1, high) else go(low, mid)
        go(0, xs.length)

case class Packet(source: Int, destination: Int, timestamp: Int):
    inline def toForward = Array(source, destination, timestamp)
    override def toString = s"{$source -> $destination @ $timestamp}"

class Router(val memoryLimit: Int):
    private val dups = Set.empty[Packet]
    private val counts = Map.empty[Int, ArrayDeque[Int]]
    private val deque = new ArrayDeque[Packet](memoryLimit)

    def addPacket(source: Int, destination: Int, timestamp: Int): Boolean =
        val packet = Packet(source, destination, timestamp)
        if dups.contains(packet) then false
        else
            if deque.size == memoryLimit then forwardPacket()
            deque.append(packet)
            dups.add(packet)
            counts.getOrElseUpdate(destination, ArrayDeque.empty).append(timestamp)
            true

    def forwardPacket(): Array[Int] =
        deque.removeHeadOption() match
            case None         => Array()
            case Some(packet) =>
                dups.remove(packet)
                counts(packet.destination).removeHead()
                packet.toForward

    def getCount(destination: Int, start: Int, end: Int): Int =
        counts.get(destination) match
            case None     => 0
            case Some(ts) =>
                ts.bisectRight(end).insertionPoint - ts.bisectLeft(start).insertionPoint

    override def toString(): String =
        s"""dups  : $dups
           |counts: $counts
           |deque : $deque
           |""".stripMargin

import munit.FunSuite

class Suite extends FunSuite:
    import upickle.default.*
    import ujson.{Value, Arr, Str, Num, Bool}

    List(
      (
        task =
            """["Router","addPacket","addPacket","addPacket","addPacket","addPacket","forwardPacket","addPacket","getCount"]""",
        value = """[[3],[1,4,90],[2,5,90],[1,4,90],[3,5,95],[4,5,105],[],[5,2,110],[5,100,110]]""",
        expected = """[null, true, true, false, true, true, [2, 5, 90], true, 1]""",
      ),
      (
        task = """["Router","addPacket","getCount"]""",
        value = """[[4],[4,5,1],[5,1,1]]""",
        expected = """[null, true, 1]""",
      ),
      (
        task = """["Router","addPacket","addPacket","getCount"]""",
        value = """[[4],[4,2,1],[3,2,1],[2,1,1]]""",
        expected = """[null, true, true, 2]""",
      ),
      (
        task =
            """["Router","addPacket","getCount","addPacket","getCount","forwardPacket","addPacket","addPacket","getCount","getCount","forwardPacket"]""",
        value = """[[4],[3,5,1],[5,1,1],[1,5,5],[5,1,2],[],[4,5,5],[2,5,5],[5,2,3],[5,4,4],[]]""",
        expected = """[null,true,1,true,1,[3,5,1],true,true,0,0,[1,5,5]]""",
      ),
    ).zipWithIndex.foreach { (input, index) =>
        test(s"Router #$index"):
            val (head, tail) =
                val ops = (read[List[String]](input.task)
                    lazyZip read[List[List[Int]]](input.value)
                    lazyZip read[List[Value]](input.expected))

                (ops.head, ops.tail)
            val router = Router(head._2(0))
            tail.foreach {
                case ("addPacket", List(source, destination, timestamp), Bool(bool)) =>
                    assertEquals(router.addPacket(source, destination, timestamp), bool, router)
                case ("forwardPacket", Nil, Arr(arr)) =>
                    assertEquals(router.forwardPacket().toList, arr.map(_.num.toInt).toList, router)
                case ("getCount", List(destination, start, end), Num(num)) =>
                    assertEquals(router.getCount(destination, start, end), num.toInt, router)
                case _ =>
            }
    }
