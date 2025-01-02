package leet.`3360`

// https://leetcode.com/problems/stone-removal-game/solutions/6076267/python3-4-lines-with-explanation-t-s-99-99/comments/2735090
object Solution:
    def canAliceWin(n: Int): Boolean =
        import math.*
        (ceil((sqrt(441 - 8 * n) - 1) / 2) % 2).toInt == 1
