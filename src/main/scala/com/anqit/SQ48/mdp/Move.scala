package com.anqit.SQ48.mdp

import com.anqit.sqala.components.Action

abstract class Move(icon: String) extends Action {
    def transform: (Int, Int)

    override def toString: String = icon
}

object Move {
    def allMoves = Set(Up, Down, Left, Right)
}

object Up extends Move("▲") {
    override def transform: (Int, Int) = (-1, 0)
}

object Down extends Move("▼") {
    override def transform: (Int, Int) = (1, 0)
}
object Left extends Move("◄") {
    override def transform: (Int, Int) = (0, -1)
}
object Right extends Move("►") {
    override def transform: (Int, Int) = (0, 1)
}