package leet.`1161`

import scala.collection.mutable.ArrayBuffer
import leet.utils.Tree, Tree.TreeNode

object Solution:
    def maxLevelSum(root: TreeNode): Int =
        val sums = ArrayBuffer.empty[Int]
        def dfs(node: TreeNode, level: Int): Unit = if node != null then
            if sums.size <= level then sums += 0
            sums(level) += node.value
            dfs(node.left, level + 1)
            dfs(node.right, level + 1)
        dfs(root, 0)
        if sums.isEmpty then 0 else sums.zipWithIndex.maxBy(_._1)._2 + 1

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,7,0,7,-8,null,null]", expected = 2),
      (input = "[989,null,10250,98693,-89388,null,null,null,-32127]", expected = 2),
    ).foreach { case (input, expected) =>
        test(s"maxLevelSum($input) = $expected"):
            assertEquals(maxLevelSum(Tree.byIndex(input)), expected)
    }
