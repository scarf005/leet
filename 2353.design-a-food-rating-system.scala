package leet.`2353`

import scala.collection.mutable.{Set, SortedSet}

case class Food(name: String, var rating: Int)
given Ordering[Food] = Ordering.by(f => (-f.rating, f.name))

extension [A](s: Set[A])
    inline def update(elem: A)(f: A => Unit) =
        s.remove(elem)
        f(elem)
        s.add(elem)

class FoodRatings(_foods: Array[String], _cuisines: Array[String], _ratings: Array[Int]):
    val db = _cuisines zip (_foods zip _ratings).map(Food.apply)
    val foods = db.iterator.map { (_, food) => food.name -> food }.toMap
    val cuisines = db.iterator.map { (cuisine, food) => food.name -> cuisine }.toMap
    val ratings = db.groupMap(_._1)(_._2).view.mapValues(SortedSet.from).toMap

    def changeRating(food: String, newRating: Int): Unit =
        ratings(cuisines(food)).update(foods(food)) { _.rating = newRating }

    def highestRated(cuisine: String): String = ratings(cuisine).head.name

import munit.FunSuite

class Suite extends FunSuite:
    import upickle.default.*
    import ujson.{Value, Arr, Str, Num}

    List(
      (
        task =
            """["FoodRatings", "highestRated", "highestRated", "changeRating", "highestRated", "changeRating", "highestRated"]""",
        value =
            """[[["kimchi", "miso", "sushi", "moussaka", "ramen", "bulgogi"], ["korean", "japanese", "japanese", "greek", "japanese", "korean"], [9, 12, 8, 15, 14, 7]], ["korean"], ["japanese"], ["sushi", 16], ["japanese"], ["ramen", 16], ["japanese"]]""",
        expected = """[null, "kimchi", "ramen", null, "sushi", null, "ramen"]""",
      ),
      (
        task =
            """["FoodRatings","changeRating","highestRated","changeRating","highestRated","highestRated","changeRating","highestRated","highestRated","changeRating","highestRated","changeRating"]""",
        value =
            """[[["tjokfmxg","xmiuwozpmj","uqklk","mnij","iwntdyqxi","cduc","cm","mzwfjk"],["waxlau","ldpiabqb","ldpiabqb","waxlau","ldpiabqb","waxlau","waxlau","waxlau"],[9,13,7,16,10,17,16,17]],[ "tjokfmxg", 19], ["waxlau"], ["uqklk", 7], ["waxlau"], ["waxlau"], ["tjokfmxg", 14], ["waxlau"], ["waxlau"], ["tjokfmxg", 4], ["waxlau"], ["mnij", 18], ["waxlau"]]""",
        expected =
            """[null, null, "tjokfmxg", null, "tjokfmxg", "tjokfmxg", null, "cduc", "cduc", null, "cduc", null, "mnij"]""",
      ),
    ).zipWithIndex.foreach { (input, index) =>
        test(s"FoodRatings #$index"):
            val task = read[List[String]](input.task)
            val value = read[List[Value]](input.value)
            val expected = read[List[Option[String]]](input.expected)

            val init = value.head.arr
            val obj = FoodRatings(
              read[Array[String]](init(0).toString),
              read[Array[String]](init(1).toString),
              read[Array[Int]](init(2).toString),
            )
            (task lazyZip value lazyZip expected).tail.foreach {
                case ("highestRated", ujson.Arr(arr), Some(str)) =>
                    assertEquals(obj.highestRated(arr.head.str), str)
                case ("changeRating", ujson.Arr(arr), _) =>
                    obj.changeRating(arr(0).str, arr(1).num.toInt)
                case _ =>
            }
    }
