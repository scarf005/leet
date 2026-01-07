package leet.utils

/** data structures to copy-paste between problems */

/** A Disjoint Set Union (DSU) structure providing near O(1) operations to merge groups (union) and
  * check element connectivity (find).
  */
class UnionFind(n: Int):
    private val parent = (0 until n).toArray
    private val size = Array.fill(n)(1)

    def find(x: Int): Int =
        if parent(x) != x then parent(x) = find(parent(x))
        parent(x)

    def union(x: Int, y: Int): Unit =
        val rootX = find(x)
        val rootY = find(y)
        if rootX != rootY then
            if size(rootX) > size(rootY) then parent(rootY) = rootX
            else if size(rootX) < size(rootY) then parent(rootX) = rootY
            else
                parent(rootY) = rootX
                size(rootX) += size(rootY)

    inline def connected(x: Int, y: Int): Boolean = find(x) == find(y)

import munit.FunSuite

class Suite extends FunSuite:
    import upickle.default.*

    List(
      (
        name = "basic union-find test",
        n = 5,
        unions = List((0, 1), (1, 2)),
        checks = List((0, 2, true), (0, 3, false)),
      ),
      (
        name = "larger union-find test",
        n = 10,
        unions = List((0, 1), (2, 3), (4, 5), (6, 7), (1, 3), (5, 7), (0, 4)),
        checks = List((0, 7, true), (0, 8, false), (3, 5, true)),
      ),
      (
        name = "multiple components test",
        n = 5,
        unions = List((0, 1), (2, 3)),
        checks = List((0, 1, true), (2, 3, true), (0, 2, false), (1, 3, false)),
      ),
      (
        name = "redundant union calls test",
        n = 3,
        unions = List((0, 1), (0, 1), (1, 0)), // if already connected
        checks = List((0, 1, true)),
      ),
    ).foreach { case (name = name, n = n, unions = unions, checks = checks) =>
        test(name):
            val uf = UnionFind(n)
            unions.foreach { case (a, b) => uf.union(a, b) }
            checks.foreach { case (a, b, expected) =>
                assertEquals(uf.connected(a, b), expected)
            }
    }

def memoize[I, O](f: (I => O)): I => O =
    val cache = collection.mutable.HashMap.empty[I, O]
    (key) => cache.getOrElseUpdate(key, f(key))

object Tree:
    class TreeNode(var value: Int = 0, var left: TreeNode = null, var right: TreeNode = null)

    def byIndex(input: String): TreeNode =
        val xs = upickle.default.read[Vector[Integer]](input)
        def go(index: Integer): TreeNode =
            if index >= xs.size || xs(index) == null then null
            else new TreeNode(xs(index), go(2 * index + 1), go(2 * index + 2))
        go(0)

    def byQueue(input: String): TreeNode =
        val xs = upickle.default.read[Vector[Integer]](input)
        if xs.isEmpty || xs(0) == null then return null
        val root = new TreeNode(xs(0))
        val queue = collection.mutable.Queue[TreeNode](root)
        var index = 1
        while index < xs.size do
            val node = queue.dequeue()
            if xs(index) != null then
                node.left = new TreeNode(xs(index))
                queue += node.left
            index += 1
            if index < xs.size && xs(index) != null then
                node.right = new TreeNode(xs(index))
                queue += node.right
            index += 1
        root
