package leet.`3200`

object Solution:
    inline def oddRow(n: Int) = n * n
    inline def evenRow(n: Int) = n * (n + 1)

    def maxHeightOfTriangle(red: Int, blue: Int) = Iterator
        .from(1)
        .find { n =>
            val odd = oddRow((n + n % 2) / 2)
            val even = evenRow((n - n % 2) / 2)

            val blueOnTop = odd <= blue && even <= red
            val redOnTop = odd <= red && even <= blue
            !(blueOnTop || redOnTop)
        }
        .get - 1

@main def main() =
    println(Solution.maxHeightOfTriangle(2, 4))
    println(Solution.maxHeightOfTriangle(2, 1))
    println(Solution.maxHeightOfTriangle(1, 1))
    println(Solution.maxHeightOfTriangle(10, 1))
