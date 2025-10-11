package leet.`3494`

object Solution:
    def minTime(skill: Array[Int], mana: Array[Int]): Long =
        inline def time(wizard: Int, potion: Int) = skill(wizard) * mana(potion)
        val times = Array.fill(skill.size)(0L)
        for j <- mana.indices do
            val curTime = skill.indices.foldLeft(0L) { (curTime, i) =>
                math.max(curTime, times(i)) + time(i, j)
            }
            times(skill.size - 1) = curTime
            for i <- skill.size - 2 to 0 by -1 do times(i) = times(i + 1) - time(i + 1, j)
        times(skill.size - 1)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (skill = "[1,5,2,4]", mana = "[5,1,4,2]", expected = 110L),
      (skill = "[1,1,1]", mana = "[1,1,1]", expected = 5L),
      (skill = "[1,2,3,4]", mana = "[1,2]", expected = 21L),
    ).foreach { case (skill, mana, expected) =>
        test(s"minTime($skill, $mana) = $expected"):
            assertEquals(minTime(read[Array[Int]](skill), read[Array[Int]](mana)), expected)
    }
