package com.anqit.SQ48

import com.anqit.SQ48.mdp._
import com.anqit.sqala.mdp.{Agent, Environment, MDP, Q}


object StuffDoer extends App {
    override def main(args: Array[String]): Unit = {
//        val manager = new GameManager(4)
//
//        p(manager)
//
//        while(!manager.over) {
//            val m =
//            print(m + "\n")
//            manager.move(m)
//            p(manager)
//        }

        var grid = Grid2();
//        p(grid)
//        while(grid.movesAvailable) {
//            val m = randomMove
//            print(m + "\n")
//            grid = grid move m
//            p(grid)
//        }

        val mdp = getMdp.learn()
        p(grid)
    }

    def getMdp = {
//        override def delta(s: S, a: A): S = d(s, a)
//        override def reward(s1: S, a: A, s2: S): Double = r(s1, a, s2)
//        override def start = startState
//        override def isTerminal(s: S): Boolean = t(s)
        def delta(grid: Grid2, m: Move) = grid move m
        def reward(orig: Grid2, m: Move, next: Grid2) = next.score - orig.score
        def start = Grid2()
        def isTerminal(grid: Grid2) = !grid.movesAvailable

        val q = Q.ndq[Grid2, Move](0.9)
        val agent = Agent.qAgent(q, Agent.randomActionSelector(Move.allMoves.toVector:_*))
        val env = Environment[Grid2, Move](reward _, delta _, start, isTerminal _)

        new MDP[Grid2, Move](agent, env)
    }

    def p(m: GameManager) = print(m + "\n-------------------------\n")

    def p(g: Grid2) = print(g + "\n-------------------------\n")

    private def randomMove =
        Move.allMoves.toVector(scala.util.Random.nextInt(Move.allMoves.size))
}
