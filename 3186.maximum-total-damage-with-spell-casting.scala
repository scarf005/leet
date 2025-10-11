package leet.`3186`

import scala.collection.SortedMap

object Solution:
    def maximumTotalDamage(power: Array[Int]): Long =
        case class Cursor(best: Long = 0L, lastPower: Int = Int.MinValue)

        val powers = power.groupMapReduce(identity)(_.toLong)(_ + _).to(SortedMap)

        var a = Cursor() // i - 3
        var b = Cursor() // i - 2
        var c = Cursor() // i - 1

        powers.foreach { case (p, totalDamage) =>
            val dontTakeDamage = c.best

            val takeDamage = totalDamage + (if p - c.lastPower > 2 then c.best
                                            else if p - b.lastPower > 2 then b.best
                                            else a.best)

            a = b; b = c; c = Cursor(best = dontTakeDamage max takeDamage, lastPower = p)
        }
        c.best

    /** 2x slower but cleaner */
    def maximumTotalDamage_slow(power: Array[Int]): Long =
        extension (m: SortedMap[Int, Long])
            inline def lastValue = m.lastOption.map(_._2).getOrElse(0L)

        val powers = power.groupMapReduce(identity)(_.toLong)(_ + _).to(SortedMap)
        val dp = collection.mutable.SortedMap.empty[Int, Long]
        powers.keys.foreach { p =>
            dp(p) = dp.lastValue max (dp.rangeTo(p - 3).lastValue + powers(p))
        }
        dp.lastValue

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,1,3,4]", expected = 6L),
      (input = "[7,1,6,6]", expected = 13L),
    ).foreach { case (input, expected) =>
        test(s"maximumTotalDamage($input) = $expected"):
            assertEquals(maximumTotalDamage(read[Array[Int]](input)), expected)
    }
