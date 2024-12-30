package leet.`2283`

object Solution:
    def digitCount(num: String) =
        val frequencies = num.groupMapReduce(_.asDigit)(_ => 1)(_ + _).withDefaultValue(0)
        num.zipWithIndex.forall((x, i) => frequencies(i) == x.asDigit)
