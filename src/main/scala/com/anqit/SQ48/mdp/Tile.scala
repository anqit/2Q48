package com.anqit.SQ48.mdp

case class Tile(var r: Int, var c: Int, var value: Int) {
    var prevR = -1
    var prevC = -1
    var mergedFrom: Option[Set[Tile]] = None

    def savePosition = {
        prevR = r
        prevC = c
    }

    def updatePosition(r: Int, c: Int) = {
        this.r = r
        this.c = c
    }

    def +(other: Tile) = {
        val t = Tile(r, c, value + other.value)
        t.mergedFrom = Some(Set(this, other))
        t
    }
    def +(m: Move) = Tile(r + m.transform._1, c + m.transform._2, value)
    def posEquals(r: Int, c: Int) = this.r == r && this.c == c
    def isEmpty = value == 0
    def isNonEmpty = !isEmpty

    override def toString: String = s"$value ($r, $c)"
}

object Tile {
    def apply(r: Int, c: Int, value: Int = 0) = new Tile(r, c, value)
}
