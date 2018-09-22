package com.anqit.SQ48

import com.anqit.SQ48.mdp._
import com.anqit.sqala.mdp.{Agent, Environment, MDP, Q}


object StuffDoer extends App {
    override def main(args: Array[String]): Unit = {
        val grid = Grid();

        val mdp = getMdp.learn()
        p(grid)
    }

    def getMdp = {
        def delta(grid: Grid, m: Move) = grid move m
        def reward(orig: Grid, m: Move, next: Grid) = next.score - orig.score
        def start = Grid()
        def isTerminal(grid: Grid) = !grid.movesAvailable

        val q = Q.ndq[Grid, Move](0.9)
        val agent = Agent.qAgent(q, Agent.randomActionSelector(Move.allMoves.toVector:_*))
        val env = Environment[Grid, Move](reward _, delta _, start, isTerminal _)

        new MDP[Grid, Move](agent, env)
    }

    def p(g: Grid) = print(g + "\n-------------------------\n")
}
