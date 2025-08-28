package leet.`3446`

object Solution:
    inline def order(h: Int) = if h > 0 then Ordering.Int else Ordering.Int.reverse

    def sortMatrix(grid: Array[Array[Int]]): Array[Array[Int]] =
        val diags = (for
            (row, y) <- grid.zipWithIndex
            (num, x) <- row.zipWithIndex
        yield (x - y) -> num).toSeq
            .groupMap(_._1)(_._2)
            .map((h, nums) => h -> nums.sorted(using order(h)))

        val res = Array.ofDim[Int](grid.size, grid.size)
        for
            (h, nums) <- diags
            (y, x) = if h > 0 then (0, h) else (-h, 0)
            i <- nums.indices
        do res(y + i)(x + i) = nums(i)
        res

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    type Input = Seq[Seq[Int]]

    def toArray(input: Input): Array[Array[Int]] = input.map(_.toArray).toArray
    def fromArray(array: Array[Array[Int]]): Input = array.map(_.toSeq).toSeq

    List(
      (input = "[[1,7,3],[9,8,2],[4,5,6]]", expected = "[[8,2,3],[9,6,7],[4,5,1]]"),
      (input = "[[0,1],[1,2]]", expected = "[[2,1],[1,0]]"),
      (input = "[[1]]", expected = "[[1]]"),
    ).foreach { case (input, expected) =>
        test(s"sortMatrix($input)"):
            assertEquals(fromArray(sortMatrix(toArray(read[Input](input)))), read[Input](expected))
    }
