package leet.`0036`

object Solution:
    def isValidSection(section: Iterable[Char]): Boolean =
        val digits = section.filter(_ != '.')
        digits.size == digits.toSet.size

    def isValidSudoku(board: Array[Array[Char]]): Boolean =
        board.forall(isValidSection)
            && board.transpose.forall(isValidSection)
            && (for
                y <- 0 until 9 by 3
                x <- 0 until 9 by 3
            yield isValidSection(for
                dy <- 0 until 3
                dx <- 0 until 3
            yield board(y + dy)(x + dx))).forall(identity)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    type Input = Array[Array[Char]]
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
        expected = true,
      ),
      (
        input = """
           [["8","3",".",".","7",".",".",".","."],
            ["6",".",".","1","9","5",".",".","."],
            [".","9","8",".",".",".",".","6","."],
            ["8",".",".",".","6",".",".",".","3"],
            ["4",".",".","8",".","3",".",".","1"],
            ["7",".",".",".","2",".",".",".","6"],
            [".","6",".",".",".",".","2","8","."],
            [".",".",".","4","1","9",".",".","5"],
            [".",".",".",".","8",".",".","7","9"]]
        """,
        expected = false,
      ),
    ).zipWithIndex.foreach { case ((input, expected), i) =>
        test(s"isValidSudoku(#${i + 1}) = $expected"):
            assertEquals(isValidSudoku(read[Input](input)), expected)
    }
