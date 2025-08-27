package leet.`3459`

object Solution:
    case class Pos(y: Int, x: Int):
        inline def +(inline that: Pos): Pos = Pos(this.y + that.y, this.x + that.x)

    enum Dir:
        case DownRight, DownLeft, UpRight, UpLeft
        inline def rotate: Dir = this match
            case DownRight => DownLeft
            case DownLeft  => UpLeft
            case UpLeft    => UpRight
            case UpRight   => DownRight

        inline def pos: Pos = this match
            case Dir.DownRight => Pos(1, 1)
            case Dir.DownLeft  => Pos(1, -1)
            case Dir.UpRight   => Pos(-1, 1)
            case Dir.UpLeft    => Pos(-1, -1)

    case class State(pos: Pos, dir: Dir, turned: Boolean):
        inline def next = State(pos + dir.pos, dir, turned)
        inline def rotate = State(pos + dir.rotate.pos, dir.rotate, turned = true)

    object State:
        inline def apply(pos: Pos, dir: Dir): State = State(pos + dir.pos, dir, turned = false)

    def lenOfVDiagonal(grid: Array[Array[Int]]): Int =
        val h = grid.length
        val w = grid.head.length

        inline def inBounds(p: Pos) = p.y >= 0 && p.y < h && p.x >= 0 && p.x < w
        inline def cell(p: Pos): Option[Int] = Option.when(inBounds(p))(grid(p.y)(p.x))

        val cache = scala.collection.mutable.Map.empty[State, Int]

        def go(s: State, expected: Int): Int = cache.getOrElseUpdate(
          s,
          cell(s.pos) match
              case Some(`expected`) =>
                  val next = 2 - expected
                  1 + math.max(go(s.next, next), if s.turned then 0 else go(s.rotate, next))
              case _ => 0,
        )

        val candidates = for
            y <- 0 until h
            x <- 0 until w
            if grid(y)(x) == 1
            dir <- Dir.values
        yield 1 + go(State(Pos(y, x), dir), expected = 2)

        candidates.maxOption.getOrElse(0)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    type Input = Array[Array[Int]]

    List(
      (input = "[[2,2,1,2,2],[2,0,2,2,0],[2,0,1,1,0],[1,0,2,2,2],[2,0,0,2,2]]", expected = 5),
      (input = "[[2,2,2,2,2],[2,0,2,2,0],[2,0,1,1,0],[1,0,2,2,2],[2,0,0,2,2]]", expected = 4),
      (input = "[[1,2,2,2,2],[2,2,2,2,0],[2,0,0,0,0],[0,0,2,2,2],[2,0,0,2,0]]", expected = 5),
      (input = "[[1]]", expected = 1),
    ).foreach { case (input, expected) =>
        val grid = read[Input](input)
        test(s"lenOfVDiagonal($input)"):
            assertEquals(lenOfVDiagonal(grid), expected)
    }
