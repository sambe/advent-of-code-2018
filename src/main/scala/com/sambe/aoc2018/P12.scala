package com.sambe.aoc2018

object P12 extends App {
  val stateSize = 1000

  val inputLines = FileUtil.loadLines("p12.in")

  val initialLine::_::rulesLines = inputLines.toList

  val initialState = initialLine.stripPrefix("initial state: ")

  case class Rule(matcher: String, result: Char)

  val rules = rulesLines.map { rl =>
    Rule(rl.substring(0, 5), rl(9))
  }.groupBy(_.matcher).mapValues(_.head.result)

  def applyRules(state: Vector[Char]): Vector[Char] = {
    val resultState = for (i <- state.indices.drop(2).dropRight(2)) yield {
      val local = state.view(i-2, i+3).mkString
      val result = rules.get(local)
      result.getOrElse(state(i))
    }
    println(state.view(stateSize-50, stateSize+150).mkString)
    Vector('.', '.') ++ resultState.toVector ++ Vector('.', '.')
  }

  val initialFullState = Vector.fill(stateSize)('.') ++ initialState.toVector ++ Vector.fill(stateSize-initialState.size)('.')

  val finalState = (1 to 20).foldLeft(initialFullState)((state,_) => applyRules(state))

  def calculateSum(state: Vector[Char]): Int = {
    state.zipWithIndex.filter(_._1 == '#').map(_._2 - stateSize).sum
  }

  val potNumberSum = calculateSum(finalState)
  println()
  println(potNumberSum)

  val futureUpTo = 100
  val states = (1 to futureUpTo).scanLeft(initialFullState)((state,_) => applyRules(state))
  val sums = states.map(calculateSum)
  println(sums)
  val sumsDiffs = ((0 +: sums) zip sums).map{case (l,r) => r - l}
  println(sumsDiffs)

  val trimmed = states.map(_.dropWhile(_ != '#').view(0, 200))
  val noChange = (trimmed zip trimmed.drop(1)).map{case(t1,t2) => t1 == t2}
  println()
  println(noChange)

  val part2Generations = 50000000000L
  val part2Sum = (part2Generations-futureUpTo) * sumsDiffs.last + sums.last
  println()
  println(part2Sum)
}
