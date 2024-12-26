package leet.`3174`

import scala.annotation.tailrec

object Solution:
    def clearDigits(s: String): String =
        val sb = StringBuilder()
        s.foreach { c =>
            if c.isDigit && sb.nonEmpty then sb.deleteCharAt(sb.size - 1) else sb.append(c)
        }
        sb.toString

@main def main() =
    import Solution.*
    println(clearDigits("cb34"))
    println(clearDigits("abc"))
