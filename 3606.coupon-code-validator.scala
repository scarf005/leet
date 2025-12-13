package leet.`3606`

object Solution:
    val lines = Set("electronics", "grocery", "pharmacy", "restaurant")
    val validCode = "[a-zA-Z0-9_]+".r
    def validateCoupons(
        code: Array[String],
        businessLine: Array[String],
        isActive: Array[Boolean],
    ): List[String] = (isActive lazyZip businessLine lazyZip code)
        .filter { (a, b, c) => validCode.matches(c) && lines.contains(b) && a }
        .sortBy(_.tail)
        .map(_.last)
        .toList

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (
        code = """["SAVE20","","PHARMA5","SAVE@20"]""",
        businessLine = """["restaurant","grocery","pharmacy","restaurant"]""",
        isActive = """[true,true,true,true]""",
        expected = """["PHARMA5","SAVE20"]""",
      ),
      (
        code = """["GROCERY15","ELECTRONICS_50","DISCOUNT10"]""",
        businessLine = """["grocery","electronics","invalid"]""",
        isActive = """[false,true,true]""",
        expected = """["ELECTRONICS_50"]""",
      ),
    ).foreach { case (code, businessLine, isActive, expected) =>
        test(s"code=$code, businessLine=$businessLine, isActive=$isActive") {
            val result = validateCoupons(
              read[Array[String]](code),
              read[Array[String]](businessLine),
              read[Array[Boolean]](isActive),
            )
            assertEquals(write(result), expected)
        }
    }
