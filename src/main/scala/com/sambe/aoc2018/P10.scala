package com.sambe.aoc2018

object P10 extends App {

  val inputLines = FileUtil.loadLines("p10.in")

  //position=<-21751,  11136> velocity=< 2, -1>
  val lineStart = "position=<"
  val lineMiddle = "> velocity=<"
  val lineEnd = ">"

  case class Light(position: (Int, Int), velocity: (Int, Int))

  val lights = inputLines.map { line =>
    val coordinates = line.stripPrefix(lineStart).stripSuffix(lineEnd).split(lineMiddle)
    def coordinateToIntPair(s: String) = {
      val numbers = s.split(",").map(_.trim.toInt)
      (numbers(0), numbers(1))
    }
    Light(coordinateToIntPair(coordinates(0)), coordinateToIntPair(coordinates(1)))
  }

  def printLights(lights: Seq[Light]) = {
    val minX: Int = lights.minBy(_.position._1).position._1
    val minY = lights.minBy(_.position._2).position._2
    val maxX = lights.maxBy(_.position._1).position._1
    val maxY = lights.maxBy(_.position._2).position._2

    if (maxX - minX > 1000 || maxY - minY > 1000) {
      println(s"too far apart: $minX..$maxX   $minY..$maxY")
    } else {
      val lightsByPosition = lights.groupBy(_.position)

      val lines = for (i <- minY to maxY) yield {
        val line = for (j <- minX to maxX) yield {
          if (lightsByPosition.contains((j, i))) '*' else ' '
        }
        line.mkString
      }
      println(lines.mkString("\n"))
    }
  }

  def moveOneStep(lights: Seq[Light]): Seq[Light] = {
    def add(p1: (Int, Int), p2: (Int, Int)): (Int, Int) = (p1._1 + p2._1, p1._2 + p2._2)
    lights.map(l => l.copy(position = add(l.position, l.velocity)))
  }

  def evaluateAlignment(lights: Seq[Light]): (Int, Int) = {
    val byColumn = lights.groupBy(_.position._1)
    val byLine = lights.groupBy(_.position._2)
    (byColumn.size, byLine.size)
  }

  (1 to 100000).foldLeft(lights){ (ls, step) =>
    val ls2 = moveOneStep(ls)
    val (alX, alY) = evaluateAlignment(ls2)
    val alignment = alX + alY
    if (alignment != 329) println(s"$step - $alignment ($alX, $alY)")
    if (alignment < 100) printLights(ls2)
    ls2
  }
}
