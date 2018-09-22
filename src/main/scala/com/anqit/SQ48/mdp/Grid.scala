package com.anqit.SQ48.mdp
import scala.util.Random
import com.anqit.sqala.components.State

class Grid(private val size: Int = 4, private val previous: Option[Grid] = None) extends State {
    require(size > 0)
    require(!previous.isDefined || previous.get.size == size)
    val grid: Array[Array[Tile]] = previous.map(Grid.from).getOrElse(Array.tabulate(size, size)(Tile(_, _)))


    def insertRandomTile(value: Int) =  randomAvailableCell.foreach(_.value = value)

    def availableCells = grid.flatten.filter(_.isEmpty)

    def isCellAvailable(t: Tile): Boolean = isCellAvailable(t.r, t.c)

    def isCellAvailable(r: Int, c: Int) = this(r, c).map(_.isEmpty).getOrElse(false)

    def areAnyCellsAvailable = !availableCells.isEmpty

    def isCellOccupied(r: Int, c: Int) = !isCellAvailable(r, c)

    def withinBounds(t: Tile): Boolean = withinBounds(t.r, t.c)

    def withinBounds(r: Int, c: Int) = r >= 0 && c >= 0 && r < size && c < size

    def insert(t: Tile) = update(t.r, t.c, t)

    def remove(t: Tile) = insert(Tile(t.r, t.c))

    def apply(r: Int, c: Int) = (r, c) match {
        case (_r, _c) if withinBounds(_r, _c) => Some(grid(_r)(_c))
        case _ => None
    }

    def update(r: Int, c: Int, t: Tile) = { grid(r)(c) = t }

    private def randomAvailableCell = {
        val cells = availableCells
        cells match {
            case Array() => None
            case _ => cells.lift(Random.nextInt(cells.size))
        }
    }

    override def toString = {
        grid.map(_.map(_.value).map("%4d".format(_)).mkString(" | ")).mkString("\n")
    }
}

object Grid {
    def empty(size: Int) = new Grid(size)

    def from(state: Grid) = state.grid.map(_.clone)
}