package leet.`1161`

import scala.collection.mutable.ArrayBuffer

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

class TreeNode(var value: Int = 0, var left: TreeNode = null, var right: TreeNode = null)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,7,0,7,-8,null,null]", expected = 2),
      (input = "[989,null,10250,98693,-89388,null,null,null,-32127]", expected = 2),
    ).foreach { case (input, expected) =>
        test(s"maxLevelSum($input) = $expected"):
            val xs = read[Vector[Integer]](input)
            def buildTree(index: Integer): TreeNode =
                if index >= xs.length || xs(index) == null then null
                else
                    val node = new TreeNode(xs(index))
                    node.left = buildTree(2 * index + 1)
                    node.right = buildTree(2 * index + 2)
                    node
            val root = buildTree(0)
            assertEquals(maxLevelSum(root), expected)
    }
