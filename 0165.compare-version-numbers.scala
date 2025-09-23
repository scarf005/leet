package leet.`0165`

extension (x: Int) inline infix def sign(y: Int) = if x < y then -1 else if x > y then 1 else 0

object Solution:
    inline def parse(version: String) = version.split('.').map(_.toInt)

    def compareVersion(a: String, b: String): Int = parse(a)
        .zipAll(parse(b), 0, 0)
        .collectFirst { case (x, y) if x != y => x sign y }
        .getOrElse(0)

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (input = (version1 = "1.2", version2 = "1.10"), expected = -1),
      (input = (version1 = "1.01", version2 = "1.001"), expected = 0),
      (input = (version1 = "1.0", version2 = "1.0.0.0"), expected = 0),
      (input = (version1 = "1.1", version2 = "1.0"), expected = 1),
    ).foreach { case ((version1, version2), expected) =>
        test(s"compareVersion($version1, $version2) = $expected"):
            assertEquals(compareVersion(version1, version2), expected)
    }
