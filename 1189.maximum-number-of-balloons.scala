package leet.`1189`

object Solution:
    def maxNumberOfBalloons(text: String): Int =
        val f = text.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)

        Vector(f('b'), f('a'), f('l') / 2, f('o') / 2, f('n')).min
