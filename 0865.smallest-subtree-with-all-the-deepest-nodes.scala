package leet.`0865`

import leet.utils.Tree, Tree.TreeNode

object Solution:
    case class Node(node: TreeNode, depth: Int)

    def getDeepest(node: TreeNode, depth: Int): Node =
        if node == null then Node(null, depth)
        else
            val left = getDeepest(node.left, depth + 1)
            val right = getDeepest(node.right, depth + 1)

            if left.depth > right.depth then left
            else if right.depth > left.depth then right
            else Node(node, left.depth)

    def subtreeWithAllDeepest(root: TreeNode): TreeNode = getDeepest(root, 0).node

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = "[3,5,1,6,2,0,8,null,null,7,4]", expected = "[2,7,4]"),
      (input = "[1]", expected = "[1]"),
      (input = "[0,1,3,null,2]", expected = "[2]"),
    ).foreach { case (input, expected) =>
        test(s"subtreeWithAllDeepest($input) = $expected"):
            assertEquals(
              subtreeWithAllDeepest(Tree.byIndex(input)),
              Tree.byIndex(expected),
            )
    }
