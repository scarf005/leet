package leet.`1912`

import collection.mutable.{Map, SortedSet}

case class Movie(shop: Int, movie: Int, price: Int):
    override def toString = s"{$movie: @$shop,$$$price}"
given Ordering[Movie] = Ordering.by(r => (r.price, r.shop, r.movie))

class MovieRentingSystem(_n: Int, _entries: Array[Array[Int]]):
    val movies = _entries.view.map { case Array(shop, movie, price) => Movie(shop, movie, price) }
    val db = movies.map { m => (m.shop, m.movie) -> m }.toMap
    val available = movies
        .groupBy(_.movie)
        .view
        .mapValues(_.to(SortedSet))
        .to(Map)
        .withDefaultValue(SortedSet.empty)
    val rented = SortedSet.empty[Movie]

    def search(movie: Int): List[Int] = available(movie).take(5).toList.map(_.shop)

    def rent(shop: Int, movie: Int): Unit =
        val m = db(shop, movie)
        rented += m
        available(movie) -= m

    def drop(shop: Int, movie: Int): Unit =
        val m = db(shop, movie)
        rented -= m
        available(movie) += m

    def report(): List[List[Int]] = rented.take(5).toList.map(m => List(m.shop, m.movie))

import munit.FunSuite

class Suite extends FunSuite:
    import upickle.default.*
    import ujson.{Arr, Value}

    inline def readList(s: Value) = read[List[Int]](s.toString)
    inline def readLists(s: Value) = read[List[List[Int]]](s.toString)
    List(
      (
        task = """["MovieRentingSystem", "search", "rent", "rent", "report", "drop", "search"]""",
        value =
            """[[3,[[0, 1, 5], [0, 2, 6], [0, 3, 7], [1, 1, 4], [1, 2, 7], [2, 1, 5]]],[1], [0, 1], [1, 2], [], [1, 2], [2]]""",
        expected = """[null, [1, 0, 2], null, null, [[0, 1], [1, 2]], null, [0, 1]]""",
      ),
      (
        task =
            """["MovieRentingSystem","rent","search","search","report","rent","rent","report","report","search","search","rent","rent","search","drop","drop","drop","drop","rent","report","report","rent","drop","search","report","drop","report","drop","rent","report","search","search","rent","rent","report","report","drop","report","report","drop","report","drop","rent","drop","search","rent","search","drop","rent","drop","report","rent","drop","rent","rent","drop","report","report","report","report","rent","drop","report","drop","rent","search","drop","report","rent","search","search","report","rent","report","report","rent","report","report","search","rent","rent","search"]""",
        value =
            """[[69,[[16,4156,1511],[20,8501,8417],[34,7901,7776],[54,6691,9511],[44,8931,8434],[42,9640,5251],[22,4534,9161],[32,6506,6831],[13,8501,731],[4,7610,8474],[33,820,2341],[17,6490,1161],[29,7120,2703],[8,8723,7613],[38,9544,1804],[30,8723,1047],[1,5015,7763],[60,1625,2383],[29,3336,3542],[39,7535,6066],[1,9074,9400],[39,1625,7944],[26,9160,6874],[55,2465,888],[35,8530,6025]]],[32,6506],[8501],[6275],[],[30,8723],[8,8723],[],[],[6699],[115],[20,8501],[16,4156],[9447],[30,8723],[8,8723],[32,6506],[16,4156],[42,9640],[],[],[17,6490],[20,8501],[8175],[],[17,6490],[],[42,9640],[54,6691],[],[1625],[3291],[60,1625],[39,1625],[],[],[60,1625],[],[],[39,1625],[],[54,6691],[8,8723],[8,8723],[2260],[29,7120],[746],[29,7120],[38,9544],[38,9544],[],[1,9074],[1,9074],[54,6691],[39,1625],[54,6691],[],[],[],[],[26,9160],[26,9160],[],[39,1625],[42,9640],[9640],[42,9640],[],[29,7120],[5630],[1842],[],[16,4156],[],[],[1,9074],[],[],[7992],[4,7610],[29,3336],[1333]]""",
        expected =
            """[null,null,[13,20],[],[[32,6506]],null,null,[[30,8723],[32,6506],[8,8723]],[[30,8723],[32,6506],[8,8723]],[],[],null,null,[],null,null,null,null,null,[[42,9640],[20,8501]],[[42,9640],[20,8501]],null,null,[],[[17,6490],[42,9640]],null,[[42,9640]],null,null,[[54,6691]],[60,39],[],null,null,[[60,1625],[39,1625],[54,6691]],[[60,1625],[39,1625],[54,6691]],null,[[39,1625],[54,6691]],[[39,1625],[54,6691]],null,[[54,6691]],null,null,null,[],null,[],null,null,null,[],null,null,null,null,null,[[39,1625]],[[39,1625]],[[39,1625]],[[39,1625]],null,null,[[39,1625]],null,null,[],null,[],null,[],[],[[29,7120]],null,[[16,4156],[29,7120]],[[16,4156],[29,7120]],null,[[16,4156],[29,7120],[1,9074]],[[16,4156],[29,7120],[1,9074]],[],null,null,[]]""",
      ),
    ).zipWithIndex.foreach { case ((input), i) =>
        val task = read[List[String]](input.task)
        val value = read[List[Arr]](input.value)
        val expected = read[List[Value]](input.expected)
        val (head, tail) =
            val ops = task lazyZip value lazyZip expected
            (ops.head, ops.tail.map { case (a, b, c) => (a, readList(b), c) })

        test(s"MovieRentingSystem #$i"):
            val movieRentingSystem = MovieRentingSystem(
              head._2(0).num.toInt,
              read[Array[Array[Int]]](head._2(1).toString),
            )
            tail.foreach {
                case ("search", List(movie), expected) =>
                    assertEquals(movieRentingSystem.search(movie), readList(expected))
                case ("rent", List(shop, movie), _) => movieRentingSystem.rent(shop, movie)
                case ("drop", List(shop, movie), _) => movieRentingSystem.drop(shop, movie)
                case ("report", Nil, expected)      =>
                    assertEquals(movieRentingSystem.report(), readLists(expected))
                case _ =>
            }
    }
