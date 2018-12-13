package com.sambe.aoc2018

import scala.collection.mutable
import scala.collection.immutable

object P13 extends App {

  val inputLines = FileUtil.loadLines("p13.in")
/*
  val inputLines =
    """
/->-\
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/
    """.split("\n").drop(1).dropRight(1)
*/
  val cartCharacters = Map('>' -> '-', '<' -> '-', '^' -> '|', 'v' -> '|')
  val cartMoves = Map(
    '>' -> (1,0),
    '<' -> (-1,0),
    '^' -> (0,-1),
    'v' -> (0,1)
  )
  val cartDirections = Map(
    ('>','-') -> '>',
    ('>','/') -> '^',
    ('>','\\') -> 'v',
    ('<','-') -> '<',
    ('<','\\') -> '^',
    ('<','/') -> 'v',
    ('^','|') -> '^',
    ('^','\\') -> '<',
    ('^','/') -> '>',
    ('v','|') -> 'v',
    ('v','\\') -> '>',
    ('v','/') -> '<'
  )
  val clockwise = Seq('>', 'v', '<', '^')

  case class Cart(location: (Int, Int), direction: Char, intersections: Int, crashed: Boolean)

  val carts = (for {
    y <- 0 until inputLines.length
    x <- 0 until inputLines(y).length
    if cartCharacters.contains(inputLines(y)(x))
  } yield {
    Cart((x, y), inputLines(y)(x), 0, crashed = false)
  }).to[immutable.IndexedSeq]

  val map =
    for (y <- 0 until inputLines.length) yield
    for (x <- 0 until inputLines(y).length) yield {
      val char = inputLines(y)(x)
      cartCharacters.getOrElse(char, char)
  }

  def add(p1: (Int, Int), p2: (Int, Int)): (Int, Int) = (p1._1 + p2._1, p1._2 + p2._2)

  def mapCharacter(p: (Int, Int)): Char = map(p._2)(p._1)

  def moveOneStep(carts: immutable.IndexedSeq[Cart]): immutable.IndexedSeq[Cart] = {
    if (carts.size == 1)
      carts // stop moving if only 1 left
    else {
      val cartPositions = mutable.Map[(Int, Int), Cart](carts.map(c => (c.location, c)): _*)
      val crashVictims = mutable.Set[Cart]()
      val movedCarts = for {
        c <- carts
      } yield {
        if (crashVictims.contains(c)) {
          c.copy(crashed = true)
        } else {
          val newLocation = add(c.location, cartMoves(c.direction))
          val newLocationChar = mapCharacter(newLocation)
          val (newDirection, newIntersections) = if (newLocationChar == '+') {
            val dirNow = clockwise.indexOf(c.direction)
            val newDir = c.intersections % 3 match {
              case 0 => (dirNow + 4 - 1) % 4
              case 1 => dirNow
              case 2 => (dirNow + 1) % 4
            }
            (clockwise(newDir), c.intersections + 1)
          } else {
            val directionAndChar = (c.direction, newLocationChar)
            (cartDirections.getOrElse(directionAndChar, sys.error("unsupported: " + directionAndChar)), c.intersections)
          }
          val crashed = cartPositions.contains(newLocation)
          if (crashed) {
            crashVictims.add(cartPositions(newLocation))
          }
          cartPositions.remove(c.location)
          cartPositions.put(newLocation, c)
          Cart(newLocation, newDirection, newIntersections, crashed)
        }
      }
      movedCarts.sortBy(_.location).to[immutable.IndexedSeq]
    }
  }

  def findCrashLocations(carts: Seq[Cart], step: Int): Seq[Crash] = {
    carts.filter(_.crashed).map(c => Crash(step, c.location)).distinct
  }

  case class Crash(move: Int, location: (Int, Int))

  val (cartsMoved, crashes) = (1 to 100000).foldLeft((carts, Seq.empty[Crash])) {
    case ((cs, crashesSoFar), step) =>
      val afterMove = moveOneStep(cs)
      val newCrashes = findCrashLocations(afterMove, step)
      val remaining = afterMove.filterNot(_.crashed)
      val crashes = if (newCrashes.nonEmpty) crashesSoFar ++ newCrashes else crashesSoFar
      (remaining, crashes)
  }
  println(crashes.mkString("\n"))
  println()
  println("last remaining: " + cartsMoved)
}
