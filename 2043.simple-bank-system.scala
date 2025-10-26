package leet.`2043`

class Bank(val balance: Array[Long]):
    extension (account: Int) inline def isValid: Boolean = account >= 1 && account <= balance.size

    def transfer(account1: Int, account2: Int, money: Long): Boolean =
        if !account1.isValid || !account2.isValid
        then false
        else if balance(account1 - 1) < money then false
        else
            balance(account1 - 1) -= money
            balance(account2 - 1) += money
            true

    def deposit(account: Int, money: Long): Boolean =
        if !account.isValid then false
        else
            balance(account - 1) += money
            true

    def withdraw(account: Int, money: Long): Boolean =
        if !account.isValid then false
        else if balance(account - 1) < money then false
        else
            balance(account - 1) -= money
            true

import munit.FunSuite

class Suite extends FunSuite:
    import upickle.default.*
    import ujson.{Arr, Value}

    inline def readList(s: Value) = read[List[Int]](s.toString)
    inline def readLongList(s: Value) = read[List[Long]](s.toString)

    List(
      (
        task = """["Bank","withdraw","transfer","deposit","transfer","withdraw"]""",
        value = """[[[10,100,20,50,30]],[3,10],[5,1,20],[5,20],[3,4,15],[10,50]]""",
        expected = """[null,true,true,true,false,false]""",
      ),
    ).zipWithIndex.foreach { case ((taskStr, valueStr, expectedStr), i) =>
        val task = read[Vector[String]](taskStr)
        val value =
            read[Vector[List[Long]]](valueStr.replaceFirst(raw"\[\[", "[").replaceFirst("]]", "]"))
        val expected = read[Vector[Boolean]](expectedStr)
        val (head, tail) =
            val ops = task lazyZip value lazyZip expected
            (ops.head, ops.tail)

        test(s"Bank #$i"):
            val bank = new Bank(head._2.toArray)
            tail.foreach:
                case ("withdraw", Seq(account, money), expected) =>
                    assertEquals(bank.withdraw(account.toInt, money), expected)
                case ("transfer", Seq(account1, account2, money), expected) =>
                    assertEquals(bank.transfer(account1.toInt, account2.toInt, money), expected)
                case ("deposit", Seq(account, money), expected) =>
                    assertEquals(bank.deposit(account.toInt, money), expected)
                case _ =>
    }
