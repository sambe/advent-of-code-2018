package com.sambe.aoc2018

object P17 extends App {

  val inputLines = FileUtil.loadLines("p17.in")

  case class Vein(from: (Int, Int), to: (Int, Int))

  val veins = inputLines.map { l =>
    val coords = l.split(",")
    val horizontal = coords(0)(0) == 'y'
    val level = coords(0).substring(2).toInt
    val fromTo = coords(1).trim.substring(2).split("\\.\\.").map(_.toInt)
    if (horizontal)
      Vein((fromTo(0), level), (fromTo(1), level))
    else
      Vein((level, fromTo(0)), (level, fromTo(1)))
  }

  val soil = Array.fill(2000, 2000)('.')

  def putClay(v: Vein): Unit = {
    for (i <- v.from._2 to v.to._2; j <- v.from._1 to v.to._1) {
      soil(i)(j) = '#'
    }
  }

  veins.foreach(putClay)

  val allY = veins.flatMap(v => Seq(v.from._2, v.to._2))
  val yMin = allY.min
  val yMax = allY.max

  waterFlow(500, 0)

  println(soil.view(0, 2000).map(_.view(400, 600).mkString).mkString("\n"))
  println()

  val totalSqmOfWater = soil.view(yMin, yMax + 1).map(_.count(sqm => sqm == '|' || sqm == '~')).sum
  println(totalSqmOfWater)

  val totalSqmOfWaterNotDrained = soil.view(yMin, yMax + 1).map(_.count(sqm => sqm == '~')).sum
  println(totalSqmOfWaterNotDrained)

  def waterFlow(x: Int, y: Int): Boolean = {
    if (y >= soil.size || soil(y)(x) == '|')
      false
    else if (soil(y)(x) == '#' || soil(y)(x) == '~') { // hit clay, go left and right
      true
    } else {
      soil(y)(x) = '|'
      val fromLeft = soil(y)(x - 1) == '|'
      val fromRight = soil(y)(x + 1) == '|'
      val blocked = waterFlow(x, y + 1)
      if (blocked) { // go left and right
      val left = if (fromLeft) true else waterFlow(x - 1, y)
        val right = if (fromRight) true else waterFlow(x + 1, y)
        val waterStanding = left && right
        if (!fromLeft && !fromRight && waterStanding) fillWaterStanding(x, y)
        waterStanding
      } else {
        false
      }
    }
  }

  def fillWaterStanding(x: Int, y: Int): Unit = {
    def goUntilClay(xx: Int, dir: Int): Unit = if (soil(y)(xx) != '#') { soil(y)(xx) = '~'; goUntilClay(xx + dir, dir) }
    goUntilClay(x, 1)
    goUntilClay(x, -1)
  }
}
