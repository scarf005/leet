package leet.`1488`

import collection.mutable
import scala.util.boundary, boundary.break

object Solution:
    inline val NO_RAIN = 0
    inline val NO_DRAIN = -1

    /** @param rains
      *   rain[i] > 0 -> lake number ${rain[i]} gets full of water on day i
      */
    def avoidFlood(rains: Array[Int]): Array[Int] = boundary:
        val res = Array.fill(rains.length)(1)
        val dryDays = mutable.SortedSet.empty[Int]
        val lakeToLastRainDay = mutable.Map.empty[Int, Int]

        for (lake, day) <- rains.zipWithIndex do
            if lake == NO_RAIN then dryDays += day
            else
                res(day) = NO_DRAIN
                lakeToLastRainDay.get(lake).map { lastRainDay =>
                    // println(s"day: $day, lake: $lake, dryDays: $dryDays, lakeToLastRainDay: $lakeToLastRainDay => lake $lake was last filled on day $lastRainDay, now on day $day, dryDays: $dryDays -> rangeFrom(lastRainDay + 1) = ${dryDays.rangeFrom(lastRainDay + 1)}",)
                    val dayToDry = dryDays.minAfter(lastRainDay).getOrElse(break(Array.empty[Int]))
                    res(dayToDry) = lake
                    dryDays -= dayToDry
                }
                lakeToLastRainDay(lake) = day
        res

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,2,3,4]", expected = "[-1,-1,-1,-1]"),
      (input = "[1,2,0,0,2,1]", expected = "[-1,-1,2,1,-1,-1]"),
      (input = "[1,2,0,1,2]", expected = "[]"),
    ).foreach { case (input, expected) =>
        test(s"avoidFlood($input)"):
            assertEquals(avoidFlood(read[Array[Int]](input)).toSeq, read[Seq[Int]](expected))
    }
