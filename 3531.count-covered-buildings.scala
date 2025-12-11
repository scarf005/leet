package leet.`3531`

extension (inline b: Boolean) inline def toInt: Int = if b then 1 else 0

object Solution:
    def countCoveredBuildings(n: Int, buildings: Array[Array[Int]]): Int =
        def by(byY: Boolean) = buildings
            .groupMap(_(byY.toInt))(_(1 - byY.toInt))
            .view
            .mapValues { xs => xs.min + 1 to xs.max - 1 }
            .toMap

        val (byY, byX) = (by(true), by(false))
        buildings.count { case Array(x, y) => byX(x).contains(y) && byY(y).contains(x) }

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (n = 3, buildings = "[[1,2],[2,2],[3,2],[2,1],[2,3]]", expected = 1),
      (n = 3, buildings = "[[1,1],[1,2],[2,1],[2,2]]", expected = 0),
      (n = 5, buildings = "[[1,3],[3,2],[3,3],[3,5],[5,3]]", expected = 1),
    ).foreach { case (n, buildings, expected) =>
        test(s"countCoveredBuildings($n, $buildings) = $expected"):
            assertEquals(countCoveredBuildings(n, read[Array[Array[Int]]](buildings)), expected)
    }
