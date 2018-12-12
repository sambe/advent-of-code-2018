package com.sambe.aoc2018

import scala.collection.mutable

object P9 extends App {

  val inputLines = FileUtil.loadLines("p9.in")

  val end = " points"
  val middle = " players; last marble is worth "

  val input = inputLines.head
  val players :: lastMarble :: Nil = input.stripSuffix(end).split(middle).map(_.toInt).toList

  case class GameState(marbles: IndexedSeq[Int], currentMarble: Int, currentPlayer: Int, scores: IndexedSeq[Int])

  def insertMarble(state: GameState, marbleValue: Int, lastMarble: Int): GameState = {
    val nextPlayer = (state.currentPlayer + 1) % players
    val newState = if (marbleValue % 23 == 0) {
      val removeIndex = (state.currentMarble + 7*state.marbles.size  - 7) % state.marbles.size
      val newMarbles = state.marbles.view(0, removeIndex) ++ state.marbles.view(removeIndex + 1, state.marbles.size)

      val newPlayerScore = state.scores(state.currentPlayer) + marbleValue + state.marbles(removeIndex)
      val updatedScores = state.scores.updated(state.currentPlayer, newPlayerScore)
      GameState(newMarbles.toIndexedSeq, removeIndex, nextPlayer, updatedScores)
    } else {
      val insertIndex = (state.currentMarble + 2) % state.marbles.size
      val newMarbles = state.marbles.view(0, insertIndex) ++ Seq(marbleValue) ++ state.marbles.view(insertIndex, state.marbles.size)
      GameState(newMarbles.toIndexedSeq, insertIndex, nextPlayer, state.scores)
    }
    if (marbleValue % 10000 == 0) println(s"${marbleValue} - ${state.scores}")
    newState
  }

  case class Marble(number: Int, var prev: Marble, var next: Marble)

  def move(steps: Int)(from: Marble): Marble = {
    (1 to steps.abs).foldLeft(from)((m,_) => if (steps > 1) m.next else m.prev)
  }

  def insert(number: Int, previous: Marble): Marble = {
    val m = Marble(number, null, null)
    val next = previous.next
    previous.next = m
    m.prev = previous
    next.prev = m
    m.next = next
    m
  }

  def remove(current: Marble): Marble = {
    val previous = current.prev
    val next = current.next
    next.prev = previous
    previous.next = next
    current.next = null
    current.prev = null
    next
  }

  def insertMarbleLinked(current: Marble, marbleToInsert: Int, player: Int, scores: mutable.Buffer[Long]): Marble = {
    if (marbleToInsert % 23 == 0) {
      val newCurrent = move(-7)(current)
      scores(player) += marbleToInsert + newCurrent.number
      remove(newCurrent)
    } else {
      val newCurrent = current.next //move(1)(current)
      insert(marbleToInsert, newCurrent)
    }
  }

  def calculatePlayerScores(players: Int, lastMarble: Int): IndexedSeq[Long] = {
    val scores = mutable.Buffer.fill(players)(0L)
    val start: Marble = Marble(0, null, null)
    start.prev = start
    start.next = start
    (1 to lastMarble).foldLeft(start) {
      (current, toInsert) =>
      val player = toInsert % players
      if (toInsert % 10000 == 0) println(s"${toInsert} - ${scores}")
      //printMarbleRing(start)(_.next)
      insertMarbleLinked(current, toInsert, player, scores)
    }
    scores.toIndexedSeq
  }

  def printMarbleRing(m: Marble)(move: Marble => Marble) = {
    val elements = mutable.Buffer[Int]()
    elements += m.number
    var current = m
    while(move(current) != m) {
      current = move(current)
      elements += current.number
    }

    println(elements.map("%2d " format _).mkString)
  }

  /*def insertMarbleLean(state: LeanGameState, marbleValue: Int, lastMarble: Int): LeanGameState = {
    val nextPlayer = (state.currentPlayer + 1) % players
    if (marbleValue % 23 == 0) {
      val removeIndex = (state.currentMarble + 7*state.marblesInCircle - 7) % state.marblesInCircle
      val newMarblesInCircle = state.marblesInCircle - 1

      val newPlayerScore = state.scores(state.currentPlayer) + marbleValue + state.marbles(removeIndex)
      val updatedScores = state.scores.updated(state.currentPlayer, newPlayerScore)
      LeanGameState()
    } else {
      LeanGameState()
    }
  }*/

  def timeIt[A](op: => A): A = {
    val start = System.currentTimeMillis()
    val result = op
    val end = System.currentTimeMillis()
    println(s"total time: ${(end-start) / 1000} seconds")
    result
  }

  //val initialState = GameState(IndexedSeq(0), 0, 0, IndexedSeq.fill(players)(0))
  //val finalState = timeIt((1 until lastMarble).foldLeft(initialState)(insertMarble(_, _, lastMarble)))

  //val winningScore = finalState.scores.max
  //println(s"Part 1: winning score: $winningScore")

  val scores = timeIt(calculatePlayerScores(players, lastMarble))
  val winningScore2 = scores.max
  println(s"Part 1 (second solution): winning score: $winningScore2")

  val scores100x = timeIt(calculatePlayerScores(players, lastMarble * 100))
  val winningScore100x = scores100x.max
  println(s"Part 2: winning score of marbles x 100: $winningScore100x")
}
