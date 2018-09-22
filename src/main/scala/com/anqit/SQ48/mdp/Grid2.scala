package com.anqit.SQ48.mdp

import com.anqit.SQ48.mdp.Grid2.{cellHasValue, valueAt, withinBounds}
import com.anqit.sqala.components.State

import scala.collection.immutable.IndexedSeq
import scala.util.Random

class Grid2 private (val grid: IndexedSeq[IndexedSeq[Int]], val score: Int) extends State {
    private val size = grid.length
    private val merged = Array.fill(size, size)(false)
    private var tilesMoved = false

    private def this(size: Int) = this(IndexedSeq.fill(size, size)(0), 0)

    def move(m: Move) = {
        var nextGridState = grid
        var scored = 0

        if(Grid2.movesAvailable(nextGridState)) {
            tilesMoved = false
            val (rTraversals, cTraversals) = buildTraversals(m)

            for {
                r <- rTraversals
                c <- cTraversals if Grid2.cellHasValue(nextGridState, r, c)
                value <- Grid2.valueAt(nextGridState, r, c)
                farthestInfo = Grid2.farthestPosition(nextGridState, r, c, m)
                ((rFarthest, cFarthest), (rNext, cNext)) = farthestInfo
            } {
                if(canMerge(nextGridState, value, rNext, cNext)) {
                    nextGridState = moveTile(nextGridState, r, c, rNext, cNext)
                    scored += Grid2.valueAt(nextGridState, rNext, cNext).get
                    merged(rNext)(cNext) = true
                } else {
                    nextGridState = moveTile(nextGridState, r, c, rFarthest, cFarthest)
                }
            }
        }

        if(tilesMoved) new Grid2(Grid2.insertRandomTile(nextGridState), score + scored) else this
    }

    private def moveTile(g: IndexedSeq[IndexedSeq[Int]], rOrig: Int, cOrig: Int, rDest: Int, cDest: Int) =
        if(!Grid2.posEquals(rOrig, cOrig, rDest, cDest)) {
            tilesMoved = true
            Grid2.remove(Grid2.setValue(g, rDest, cDest, Grid2.valueAt(g, rDest, cDest).get + Grid2.valueAt(g, rOrig, cOrig).get), rOrig, cOrig)
        } else {
            g
        }
    private def insertRandomTile(): Grid2 = new Grid2(Grid2.insertRandomTile(grid), score)

    private def canMerge(g: IndexedSeq[IndexedSeq[Int]], value: Int, rTo: Int, cTo: Int) =
        cellHasValue(g, rTo, cTo) && isCellUnmerged(g, rTo, cTo) && valueAt(g, rTo, cTo).contains(value)
    private def isCellUnmerged(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int) = withinBounds(g, r, c) && !merged(r)(c)

    private def buildTraversals(m: Move) = {
        var rTraversals = 0 until size
        var cTraversals = 0 until size

        if(m.transform._1 == 1) rTraversals = rTraversals.reverse
        if(m.transform._2 == 1) cTraversals = cTraversals.reverse

        (rTraversals, cTraversals)
    }

    def movesAvailable: Boolean = Grid2.movesAvailable(grid)

    def apply(r: Int, c: Int) = Grid2.valueAt(grid, r, c)

    override def toString = grid.map(_.map("%4d".format(_)).mkString(" | ")).mkString("\n") + s"\nScore: $score"

    override def equals(obj: Any): Boolean = obj match {
            case that: Grid2 => (that canEqual this) && this.score == that.score && this.grid == that.grid
            case _ => false
        }
    override def hashCode(): Int = 41 * (41 + score) + grid.hashCode()
    def canEqual(other: Any) = other.isInstanceOf[Grid2]
}

object Grid2 {
    private val EMPTY_VAL = 0

    def apply(size: Int = 4) = new Grid2(size).insertRandomTile().insertRandomTile()

    private def insertRandomTile(g: IndexedSeq[IndexedSeq[Int]]) =
        randomAvailableCell(g).map(p => setValue(g, p._1, p._2, if(Random.nextFloat() < 0.9 ) 2 else 4)).getOrElse(g)
    private def setValue(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int, value: Int) = g.updated(r, g(r).updated(c, value))
    private def remove(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int) = setValue(g, r, c, Grid2.EMPTY_VAL)

    private def valueAt(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int) = (r, c) match {
        case (_r, _c) if withinBounds(g, _r, _c) => Some(g(_r)(_c))
        case _ => None
    }
    private def randomAvailableCell(g: IndexedSeq[IndexedSeq[Int]]) = {
        val cells = availableCells(g)
        cells match {
            case IndexedSeq() => None
            case _ => cells.lift(Random.nextInt(cells.size))
        }
    }
    private def availableCells(g: IndexedSeq[IndexedSeq[Int]]) = for {
        r <- g.indices
        c <- g(r).indices if isCellAvailable(g, r, c)
    } yield (r, c)

    private def movesAvailable(g: IndexedSeq[IndexedSeq[Int]]) = areAnyCellsAvailable(g) || areAnyMatchesAvailable(g)
    private def cellHasValue(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int) = valueAt(g, r, c).isDefined && !valueAt(g, r, c).contains(Grid2.EMPTY_VAL)
    private def isCellAvailable(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int) = valueAt(g, r, c).contains(Grid2.EMPTY_VAL)
    private def isCellUnavailable(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int) = !isCellAvailable(g, r, c)
    private def withinBounds(g: IndexedSeq[IndexedSeq[Int]], r: Int , c: Int) = r >= 0 && c >= 0 && r < g.size && c < g.size
    private def areAnyCellsAvailable(g: IndexedSeq[IndexedSeq[Int]]) = availableCells(g).nonEmpty
    private def areAnyMatchesAvailable(g: IndexedSeq[IndexedSeq[Int]]): Boolean = {
        for {
            r <- g.indices
            c <- g(r).indices if cellHasValue(g, r, c)
            value <- valueAt(g, r, c)
            (dr, dc) <- Move.allMoves.map(_.transform)
            _ <- valueAt(g, r + dr, c + dc).filter(_ == value)
        } {
            return true
        }

        false
    }
    private def posEquals(r1: Int, c1: Int, r2: Int, c2: Int) = r1 == r2 && c1 == c2

    private def farthestPosition(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int, m: Move): ((Int, Int), (Int, Int)) = {
        val (dr, dc) = m.transform
        val (rNext, cNext) = (r + dr, c + dc)

        if(isCellUnavailable(g, rNext, cNext)) {
            ((r, c), (rNext, cNext))
        } else {
            farthestPosition(g, rNext, cNext, m)
        }
    }
}