package com.sambe.aoc2018

object P11 extends App {

  val inputLines = FileUtil.loadLines("p11.in")

  val gridSerialNumber = inputLines.head.toInt
  val size = 300

  //The power level in a given fuel cell can be found through the following process:
  //
  //  * Find the fuel cell's rack ID, which is its X coordinate plus 10.
  //  * Begin with a power level of the rack ID times the Y coordinate.
  //  * Increase the power level by the value of the grid serial number (your puzzle input).
  //  * Set the power level to itself multiplied by the rack ID.
  //  * Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
  //  * Subtract 5 from the power level.

  def calculatePowerLevel(x: Int, y: Int, gridSerial: Int): Int = {
    val rackId = x + 10
    val powerLevel1 = (rackId * y + gridSerial) * rackId
    val hundredsDigit = powerLevel1 / 100 % 10
    hundredsDigit - 5
  }

  println(calculatePowerLevel(122,79, 57)) // should be -5
  println(calculatePowerLevel(217,196, 39)) // should be 0
  println(calculatePowerLevel(101,153, 71)) // should be 4

  val powerLevels = for (y <- 1 to size) yield {
    for (x <- 1 to size) yield {
      calculatePowerLevel(x, y, gridSerialNumber)
    }
  }
  println(powerLevels.view(0, 10).map(_.view(0, 10).map("%3d ".format(_)).mkString).mkString("\n"))
  println()

  val xAccumulatedPowerLevels = powerLevels.map(_.scan(0)((acc, n) => acc + n))
  println(xAccumulatedPowerLevels.view(0, 10).map(_.view(0, 10).map("%3d ".format(_)).mkString).mkString("\n"))
  println()

  val squaresSums = for (k <- 1 to 300) yield {
    println(s"starting $k")
    for (i <- 0 to size - k) yield {
      for (j <- 0 to size - k) yield {
        xAccumulatedPowerLevels.view(i, i + k).map { accumulatedLevelsRow =>
          accumulatedLevelsRow(j+k) - accumulatedLevelsRow(j)
          //levelsRow.view(j, j + k).sum
        }.sum
      }
    }
  }
  println("squaresSums")
  println(squaresSums(3-1).view(0, 10).map(_.view(0, 10).map("%3d ".format(_)).mkString).mkString("\n"))
  println()

  def maximumKSquare(k: Int): (Int, (Int, Int)) = {
    val ((maximum, maxX), maxY) = squaresSums(k-1).view.map(_.view.zipWithIndex.maxBy(_._1)).zipWithIndex.maxBy(_._1._1)
    (maximum, (maxX+1, maxY+1))
  }
  //val ((maximum3x3, maxX), maxY) = squaresSums(3-1).view.map(_.view.zipWithIndex.maxBy(_._1)).zipWithIndex.maxBy(_._1._1)
  val (maximum3x3, (max3X, max3Y)) = maximumKSquare(3)
  println(s"Part 1: $maximum3x3 at $max3X,$max3Y")
  println()

  val maximums = (1 to size).map(k => (maximumKSquare(k), k))
  val ((maximum, (maxX, maxY)), maxK) = maximums.maxBy(_._1._1)
  println(maximums.mkString("\n"))
  println(s"Part 2: $maximum at $maxX,$maxY,$maxK")
}
