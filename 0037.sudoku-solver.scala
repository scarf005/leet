package leet.`0037`

import scala.util.boundary, boundary.break

type Board = Array[Array[Char]]

object Solution:
    inline def section(inline n: Int): Int = (n / 3) * 3
    inline def isValid(board: Board, y: Int, x: Int, digit: Char): Boolean =
        !board(y).contains(digit) &&
            !(0 until 9).exists(board(_)(x) == digit) &&
            (for r <- 0 until 3; c <- 0 until 3
            yield board(r + section(y))(c + section(x))).forall(_ != digit)

    def solveSudoku(board: Board): Unit =
        def go(): Boolean = boundary:
            for
                y <- 0 until 9
                x <- 0 until 9
                if board(y)(x) == '.'
            do
                for d <- '1' to '9' if isValid(board, y, x, d) do
                    board(y)(x) = d
                    if go() then break(true)
                    board(y)(x) = '.'
                break(false)
            true
        go()

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    extension (grid: Board) def fmt: String = grid.map(_.mkString).mkString("\n")

    List(
      (
        input = """
           [["5","3",".",".","7",".",".",".","."],
            ["6",".",".","1","9","5",".",".","."],
            [".","9","8",".",".",".",".","6","."],
            ["8",".",".",".","6",".",".",".","3"],
            ["4",".",".","8",".","3",".",".","1"],
            ["7",".",".",".","2",".",".",".","6"],
            [".","6",".",".",".",".","2","8","."],
            [".",".",".","4","1","9",".",".","5"],
            [".",".",".",".","8",".",".","7","9"]]
        """,
        expected = """
           [["5","3","4","6","7","8","9","1","2"],
            ["6","7","2","1","9","5","3","4","8"],
            ["1","9","8","3","4","2","5","6","7"],
            ["8","5","9","7","6","1","4","2","3"],
            ["4","2","6","8","5","3","7","9","1"],
            ["7","1","3","9","2","4","8","5","6"],
            ["9","6","1","5","3","7","2","8","4"],
            ["2","8","7","4","1","9","6","3","5"],
            ["3","4","5","2","8","6","1","7","9"]]
        """,
      ),
    ).zipWithIndex.foreach { case ((input, expected), i) =>
        test(s"solveSudoku #$i"):
            val board = read[Board](input)
            solveSudoku(board)
            assertEquals(board.fmt, read[Board](expected).fmt)
    }
