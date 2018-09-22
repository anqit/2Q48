package com.anqit.SQ48.mdp
import scala.util.Random
import scala.collection.immutable.IndexedSeq
import com.anqit.sqala.components.State

class Grid2 private (private val grid: IndexedSeq[IndexedSeq[Int]], val score: Int) extends State {
    private val size = grid.length
    private val merged = Array.fill(size, size)(false)
    private var tilesMoved = false

    private def this(size: Int) = {
        this(IndexedSeq.fill(size, size)(0), 0)
        addStartTiles()
    }

    private def addStartTiles() = for ( _ <- 1 to 2) insertRandomTile()

    def move(m: Move) = {
        var nextGridState = grid
        var scored = 0

        if(movesAvailable) {
            tilesMoved = false
            val (rTraversals, cTraversals) = buildTraversals(m)

            for {
                r <- rTraversals
                c <- cTraversals if hasValue(nextGridState, r, c)
                value = nextGridState(r)(c)
                farthestInfo = farthestPosition(nextGridState, r, c, m)
                ((rFarthest, cFarthest), (rNext, cNext)) = farthestInfo
            } {
                if(canMerge(nextGridState, value, rNext, cNext)) {
                    nextGridState = moveTile(nextGridState, r, c, rNext, cNext)
                    scored += nextGridState(rNext)(cNext)
                    merged(rNext)(cNext) = true
                } else {
                    nextGridState = moveTile(nextGridState, r, c, rFarthest, cFarthest)
                }
            }
        }

        if(tilesMoved) new Grid2(insertRandomTile(nextGridState), score + scored) else this
    }

    private def moveTile(g: IndexedSeq[IndexedSeq[Int]], rOrig: Int, cOrig: Int, rDest: Int, cDest: Int) = {
        if(!posEquals(rOrig, cOrig, rDest, cDest)) {
            tilesMoved = true
            remove(setValue(g, rDest, cDest, g(rDest)(cDest) + g(rOrig)(cOrig)), rOrig, cOrig)
        } else {
            g
        }
    }
    private def insertRandomTile(g: IndexedSeq[IndexedSeq[Int]]) =  randomAvailableCell(g).map(p => setValue(g, p._1, p._2, if(Random.nextFloat() < 0.9 ) 2 else 4)).getOrElse(g)

    private def setValue(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int, value: Int) = g.updated(r, g(r).updated(c, value))
    private def remove(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int) = setValue(g, r, c, Grid2.EMPTY_VAL)

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

    private def hasValue(g: IndexedSeq[IndexedSeq[Int]], r: Int , c: Int) = withinBounds(g, r, c) && g(r)(c) != Grid2.EMPTY_VAL
    private def isCellAvailable(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int) = withinBounds(g, r, c) && g(r)(c) == Grid2.EMPTY_VAL
    private def isCellUnavailable(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int) = !isCellAvailable(g, r, c)
    private def canMerge(g: IndexedSeq[IndexedSeq[Int]], value: Int, rTo: Int, cTo: Int) =
        value != Grid2.EMPTY_VAL && !wasCellMerged(g, rTo, cTo) && g(rTo)(cTo) == value
    private def wasCellMerged(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int) = withinBounds(g, r, c) && merged(r)(c)
    private def withinBounds(g: IndexedSeq[IndexedSeq[Int]], r: Int , c: Int) = r >= 0 && c >= 0 && r < g.size && c < g.size

    private def areAnyMatchesAvailable: Boolean = {
        for {
            r <- grid.indices
            c <- grid(r).indices if isCellUnavailable(r, c)
            value <- this(r, c)
            (dr, dc) <- Move.allMoves.map(_.transform)
            _ <- this(r + dr, c + dc).filter(_ == value)
        } {
            return true
        }

        false
    }

    def movesAvailable = areAnyCellsAvailable || areAnyMatchesAvailable

    private def posEquals(r1: Int, c1: Int, r2: Int, c2: Int) = r1 == r2 && c1 == c2
    private def buildTraversals(m: Move) = {
        var rTraversals = 0 until size
        var cTraverslas = 0 until size

        if(m.transform._1  == 1) rTraversals = rTraversals.reverse
        if(m.transform._2  == 1) cTraverslas = cTraverslas.reverse

        (rTraversals, cTraverslas)
    }

    private def insertRandomTile() =  randomAvailableCell.foreach(p => this(p._1, p._2) = if(Random.nextFloat() < 0.9 ) 2 else 4)

    private def availableCells = for {
            r <- grid.indices
            c <- grid(r).indices if isCellAvailable(r, c)
        } yield (r, c)

    private def areAnyCellsAvailable = availableCells.nonEmpty

    private def isCellAvailable(r: Int, c: Int) = this(r, c).contains(Grid2.EMPTY_VAL)
    private def isCellUnavailable(r: Int, c: Int) = !isCellAvailable(r, c)

    private def withinBounds(r: Int, c: Int) = r >= 0 && c >= 0 && r < size && c < size

    private def randomAvailableCell = {
        val cells = availableCells
        cells match {
            case IndexedSeq() => None
            case _ => cells.lift(Random.nextInt(cells.size))
        }
    }

    private def farthestPosition(g: IndexedSeq[IndexedSeq[Int]], r: Int, c: Int, m: Move): ((Int, Int), (Int, Int)) = {
        val (dr, dc) = m.transform
        val (rNext, cNext) = (r + dr, c + dc)

        if(isCellUnavailable(g, rNext, cNext)) {
            ((r, c), (rNext, cNext))
        } else {
            farthestPosition(g, rNext, cNext, m)
        }
    }

    def apply(r: Int, c: Int) = (r, c) match {
        case (_r, _c) if withinBounds(_r, _c) => Some(grid(_r)(_c))
        case _ => None
    }

    override def toString = {
        grid.map(_.map("%4d".format(_)).mkString(" | ")).mkString("\n") + s"\nScore: $score"
    }

    override def equals(obj: Any): Boolean = obj match {
            case that: Grid2 => (that canEquals this) && this.score == that.score && this.grid == that.grid
            case _ => false
        }

    override def hashCode(): Int = {
        41 * (41 + score) + grid.hashCode()
    }

    def canEquals(other: Any) = other.isInstanceOf[Grid2]
}

object Grid2 {
    private val EMPTY_VAL = 0

    def apply(size: Int = 4) = new Grid2(size)
}