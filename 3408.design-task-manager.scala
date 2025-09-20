package leet.`3408`

import scala.collection.mutable.{Set, SortedSet, Map}

extension [A](s: Set[A])
    inline def update(elem: A)(f: A => Unit) =
        s.remove(elem)
        f(elem)
        s.add(elem)

case class Task(userId: Int, taskId: Int, var priority: Int)
given Ordering[Task] = Ordering.by(t => (t.priority, t.taskId))

class TaskManager(_tasks: List[List[Int]]):
    val tasks = _tasks.collect { case List(userId, taskId, priority) =>
        Task(userId, taskId, priority)
    }
    val dict = tasks.map(t => t.taskId -> t).to(Map)
    val db = SortedSet.from(tasks)

    def add(userId: Int, taskId: Int, priority: Int): Unit =
        val task = Task(userId, taskId, priority)
        dict(taskId) = task
        db += task

    def edit(taskId: Int, newPriority: Int): Unit =
        val task = dict(taskId)
        db.update(task) { _.priority = newPriority }

    def rmv(taskId: Int): Unit =
        val task = dict(taskId)
        db.remove(task)
        dict.remove(taskId)

    def execTop(): Int = db.maxOption
        .map { task =>
            db.remove(task)
            dict.remove(task.taskId)
            task.userId
        }
        .getOrElse(-1)

import munit.FunSuite

class Suite extends FunSuite:
    import upickle.default.*
    import ujson.{Value, Arr, Str, Num}

    List(
      (
        task = """["TaskManager","add","edit","execTop","rmv","add","execTop"]""",
        value =
            """[[[[1, 101, 10], [2, 102, 20], [3, 103, 15]]], [4, 104, 5], [102, 8], [], [101], [5, 105, 15], []]""",
        expected = "[null, null, null, 3, null, null, 5]",
      ),
    ).zipWithIndex.foreach { (input, index) =>
        test(s"TaskManager #$index"):
            val task = read[List[String]](input.task)
            val value = read[List[Arr]](input.value)
            val expected = read[List[Option[Int]]](input.expected)

            val initValue = read[List[List[Int]]](value.head.arr.head.toString)
            val manager = TaskManager(initValue)
            (task lazyZip value lazyZip expected).tail.foreach {
                case ("add", Arr(arr), _) =>
                    manager.add(arr(0).num.toInt, arr(1).num.toInt, arr(2).num.toInt)
                case ("edit", Arr(arr), _) =>
                    manager.edit(arr(0).num.toInt, arr(1).num.toInt)
                case ("rmv", Arr(arr), _)           => manager.rmv(arr(0).num.toInt)
                case ("execTop", Arr(_), Some(num)) =>
                    assertEquals(manager.execTop(), num)
                case _ =>
            }
    }
