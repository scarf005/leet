package leet.`3484`

import collection.mutable.Map

case class Cell(col: Char, row: Int)

object Cell:
    def apply(cell: String): Cell = Cell(cell.head, cell.tail.toInt)

class Spreadsheet(_rows: Int):
    val cells = Map.empty[Cell, Int].withDefaultValue(0)

    object Val:
        def unapply(value: String): Option[Int] =
            Some(if value.head.isLetter then cells(Cell(value)) else value.toInt)

    def setCell(cell: String, value: Int): Unit = cells(Cell(cell)) = value

    def resetCell(cell: String): Unit = cells.remove(Cell(cell))

    def getValue(formula: String): Int = formula match
        case s"=${Val(a)}+${Val(b)}" => a + b

import munit.FunSuite

class Suite extends FunSuite:
    import upickle.default.*
    import ujson.{Value, Arr, Str, Num}

    List(
      (
        task =
            """["Spreadsheet", "getValue", "setCell", "getValue", "setCell", "getValue", "resetCell", "getValue"]""",
        value =
            """[[3], ["=5+7"], ["A1", 10], ["=A1+6"], ["B2", 15], ["=A1+B2"], ["A1"], ["=A1+B2"]]""",
        expected = "[null, 12, null, 16, null, 25, null, 15] ",
      ),
    ).zipWithIndex.foreach { (input, index) =>
        test(s"Spreadsheet #$index"):
            val task = read[List[String]](input.task)
            val value = read[List[Arr]](input.value)
            val expected = read[List[Option[Int]]](input.expected)

            val sheet = Spreadsheet(0)
            (task zip (value zip expected)).foreach {
                case ("getValue", (Arr(arr), Some(num))) =>
                    assertEquals(sheet.getValue(arr.head.str), num)
                case ("setCell", (Arr(arr), _)) =>
                    sheet.setCell(arr.head.str, arr(1).num.toInt)
                case ("resetCell", (Arr(arr), _)) => sheet.resetCell(arr.head.str)
                case _                            =>
            }
    }
