package leet.`0407`

import collection.mutable.{Set, PriorityQueue}

case class Pos(x: Int, y: Int)(using heightMap: Array[Array[Int]]):
    lazy val z: Int = heightMap(x)(y)
    inline def neighbors: List[Pos] =
        List(Pos(x - 1, y), Pos(x + 1, y), Pos(x, y - 1), Pos(x, y + 1))

given Ordering[Pos] = Ordering.by(-_.z)

object Solution:
    def trapRainWater(heightMap: Array[Array[Int]]): Int =
        given map: Array[Array[Int]] = heightMap

        val width = heightMap(0).length
        val height = heightMap.length

        inline def isEdge(p: Pos): Boolean =
            p.x == 0 || p.x == height - 1 || p.y == 0 || p.y == width - 1
        inline def inBounds(p: Pos): Boolean =
            p.x >= 0 && p.x < height && p.y >= 0 && p.y < width
        extension (heightMap: Array[Array[Int]])
            def apply(p: Pos): Int = heightMap(p.x)(p.y)
            def update(p: Pos, v: Int): Unit = heightMap(p.x)(p.y) = v

        val visited =
            ((0 until height).iterator.flatMap { x => Seq(Pos(x, 0), Pos(x, width - 1)) }
                ++ (0 until width).iterator.flatMap { y => Seq(Pos(0, y), Pos(height - 1, y)) })
                .to(Set)

        val queue = visited.to(PriorityQueue)

        var water = 0
        while queue.nonEmpty do
            val pos = queue.dequeue()

            pos.neighbors
                .filter { p => inBounds(p) && !visited.contains(p) }
                .foreach { np =>
                    visited.add(np)
                    val nh = heightMap(np)
                    water += 0 max pos.z - nh
                    heightMap(np) = math.max(nh, pos.z)
                    queue.enqueue(np)
                }
        water

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[[1,4,3,1,3,2],[3,2,1,3,2,4],[2,3,3,2,3,1]]", expected = 4),
      (input = "[[3,3,3,3,3],[3,2,2,2,3],[3,2,1,2,3],[3,2,2,2,3],[3,3,3,3,3]]", expected = 10),
    ).foreach { case (input, expected) =>
        test(s"trapRainWater(${input}) = ${expected}"):
            assertEquals(trapRainWater(read[Array[Array[Int]]](input)), expected)
    }
