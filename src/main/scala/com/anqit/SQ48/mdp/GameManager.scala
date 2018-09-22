package com.anqit.SQ48.mdp

import scala.util.Random

class GameManager(val size: Int = 4) {
//    val grid = Grid.empty(size)
    var score = 0
    var over = false
    var won = false
    var continue = false

    def move(grid: Grid, m: Move) = {
        var scoreDelta = 0

        if(!isGameTerminated) {
            val traversals = buildTraversals(m)
            var moved = false
            clearMergeInfo(grid)

            for {
                r <- traversals._1
                c <- traversals._2
                tile <- grid(r, c).filter(_.isNonEmpty)
            } {
                val (farthest, next) = farthestPostion(grid, tile, m)

                if(next.isNonEmpty && next.mergedFrom.isEmpty && next.value == tile.value) {
                    val merged = next + tile
                    grid insert merged
                    grid remove tile

                    tile.updatePosition(next.r, next.c)
                    scoreDelta += merged.value
                } else {
                    moveTile(grid, tile, farthest)
                }

                moved |= !tile.posEquals(r, c)
            }

            if(moved) {
                addRandomTile(grid)
                over = !movesAvailable(grid)
            }
        }

        score += scoreDelta
        scoreDelta
    }

    private def moveTile(grid: Grid, orig: Tile, dest: Tile) = {
        grid remove orig
        grid(dest.r, dest.c) = orig
        orig.updatePosition(dest.r, dest.c)
    }

    private def isGameTerminated = over || (won && !continue)

    private def addStartTiles(grid: Grid) = {
        for ( _ <- 1 to 2) addRandomTile(grid)
    }

    private def addRandomTile(grid: Grid) = {
        if(grid.areAnyCellsAvailable) {
            grid.insertRandomTile(if(Random.nextFloat() < 0.9 ) 2 else 4)
        }
    }

    private def buildTraversals(m: Move) = {
        var rTraversals = 0 until size
        var cTraverslas = 0 until size

        if(m.transform._1  == 1) rTraversals = rTraversals.reverse
        if(m.transform._2  == 1) cTraverslas = cTraverslas.reverse

        (rTraversals, cTraverslas)
    }

    private def movesAvailable(grid: Grid) = grid.areAnyCellsAvailable || matchesAvailable(grid)

    private def matchesAvailable(grid: Grid): Boolean = {
        for {
            r <- 0 until size
            c <- 0 until size
            t <- grid(r, c).filter(_.isNonEmpty)
            v <- Move.allMoves.map(_.transform)
        } {
            if(grid(r + v._1, c + v._2).filter(_.value == t.value).isDefined)
                return true
        }

        return false
    }

    private def farthestPostion(grid: Grid, t: Tile, m: Move): (Tile, Tile) = {
        val vector = m.transform
        val next = grid(t.r + vector._1, t.c + vector._2)

        if(!next.filter(grid.withinBounds _).filter(grid.isCellAvailable _).isDefined) {
            (t, next.getOrElse(Tile(-1, -1, 0)))
        } else {
            farthestPostion(grid, next.get, m)
        }
    }

    private def clearMergeInfo(grid: Grid) = {
        grid.grid.foreach(_.foreach(_.mergedFrom = None))
    }

//    override def toString: String = grid.toString + s"\nScore: $score"
}
