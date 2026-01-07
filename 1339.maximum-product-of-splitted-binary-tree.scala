package leet.`1339`
import leet.utils.Tree, Tree.TreeNode

import scala.collection.mutable.ArrayBuffer

inline val MOD = 1_000_000_007
object Solution:
    def maxProduct(root: TreeNode): Int =
        val xs = ArrayBuffer.empty[Long]
        def dfs(node: TreeNode): Long = if node == null then 0L
        else
            val sum = node.value.toLong + dfs(node.left) + dfs(node.right)
            xs += sum
            sum
        val total = dfs(root)
        (xs.map { s => (total - s) * s }.max % MOD).toInt

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[1,2,3,4,5,6]", expected = 110),
      (input = "[1,null,2,3,4,null,null,5,6]", expected = 90),
    ).foreach { case (input, expected) =>
        test(s"maxProduct($input) = $expected"):
            assertEquals(maxProduct(Tree.byQueue(input)), expected)
    }
