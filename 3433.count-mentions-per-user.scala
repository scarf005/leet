package leet.`3433`

import collection.mutable
import scala.util.Try

object Int:
    def unapply(s: String): Option[Int] = Try(s.toInt).toOption

enum Noti:
    case All
    case Here
    case Mentions(ids: Vector[Int])

object Noti:
    def unapply(s: String): Option[Noti] = Some(s match
        case "ALL"  => All
        case "HERE" => Here
        case other  => Mentions(other.split(" ").toVector.map(_.replace("id", "").toInt)))

object Solution:
    def countMentions(numberOfUsers: Int, events: List[List[String]]): Array[Int] =
        // index = user ID, value = online after timestamp, initialally all 0 (online)
        val allIds = (0 until numberOfUsers)
        val onlineSince = Array.fill(numberOfUsers)(0)
        val mentions = Array.fill(numberOfUsers)(0)
        extension (ids: Iterable[Int]) def mention() = ids.foreach { id => mentions(id) += 1 }
        events.sortBy(e => (e(1).toInt, if e(0) == "OFFLINE" then 0 else 1)).foreach {
            case List("MESSAGE", Int(timestamp), Noti(noti)) =>
                noti match
                    case Noti.Mentions(ids) => ids.mention()
                    case Noti.All           => allIds.mention()
                    case Noti.Here          =>
                        allIds.filter(id => onlineSince(id) <= timestamp).mention()
            case List("OFFLINE", Int(timestamp), Int(id)) => onlineSince(id) = timestamp + 60
            case none                                     => println(s"Unknown event: $none")
        }
        mentions.toArray

import munit.FunSuite

class Suite extends FunSuite:
    import Solution.*
    import upickle.default.*

    List(
      (
        numberOfUsers = 2,
        events = """[["MESSAGE","10","id1 id0"],["OFFLINE","11","0"],["MESSAGE","71","HERE"]]""",
        expected = "[2,2]",
      ),
      (
        numberOfUsers = 2,
        events = """[["MESSAGE","10","id1 id0"],["OFFLINE","11","0"],["MESSAGE","12","ALL"]]""",
        expected = "[2,2]",
      ),
      (
        numberOfUsers = 2,
        events = """[["OFFLINE","10","0"],["MESSAGE","12","HERE"]]""",
        expected = "[0,1]",
      ),
      (
        numberOfUsers = 3,
        events =
            """[["MESSAGE","2","HERE"],["OFFLINE","2","1"],["OFFLINE","1","0"],["MESSAGE","61","HERE"]]""",
        expected = "[1,0,2]",
      ),
    ).foreach { case (numberOfUsers, events, expected) =>
        test(s"countMentions($numberOfUsers, $events)"):
            assertEquals(
              countMentions(numberOfUsers, read[List[List[String]]](events)).toVector,
              read[Vector[Int]](expected),
            )
    }
