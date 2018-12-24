package com.sambe.aoc2018

object P18 extends App {

  val initialMap = FileUtil.loadLines("p18.in")

  def calculateRound(unpaddedMap: Seq[String]): Seq[String] = {
    val n = 50
    val paddingRow = Seq.fill(n + 2)(' ').mkString
    val map = paddingRow +: unpaddedMap.map(' ' + _ + ' ') :+ paddingRow
    val updated = for (i <- 1 to n) yield {
      for (j <- 1 to n) yield {
        val upper = map(i - 1).substring(j - 1, j + 2).toSeq
        val middle = Seq(map(i)(j - 1), map(i)(j + 1))
        val lower = map(i + 1).substring(j - 1, j + 2).toSeq
        val adjacentCells = upper ++ middle ++ lower
        def countAll(char: Char): Int = adjacentCells.count(_ == char)

        val open = countAll('.')
        val trees = countAll('|')
        val lumberyard = countAll('#')

        map(i)(j) match {
          case '.' => if (trees >= 3) '|' else '.'
          case '|' => if (lumberyard >= 3) '#' else '|'
          case '#' => if (lumberyard >= 1 && trees >= 1) '#' else '.'
        }
      }
    }
    //println(updated.map(_.mkString).mkString("\n"))
    //println()
    updated.map(_.mkString)
  }

  /*val finalMap = (1 to 10).foldLeft(initialMap.toSeq)((map, _) => calculateRound(map))
  println(finalMap.mkString("\n"))
  println()

  val trees = finalMap.map(_.count(_ == '|')).sum
  val lumberyards = finalMap.map(_.count(_ == '#')).sum
  val resourceValue = trees * lumberyards
  println(resourceValue)*/

  def calculateResourceValue(input: (Seq[String], Vector[Int])): (Seq[String], Vector[Int]) = {
    val newMap = calculateRound(input._1)
    val trees = newMap.map(_.count(_ == '|')).sum
    val lumberyards = newMap.map(_.count(_ == '#')).sum
    val resourceValue = trees * lumberyards
    (newMap, input._2 :+ resourceValue)
  }

  val (finalMap, resourceValues: Vector[Int]) =
    (1 to 1000).fold((initialMap.toSeq, Vector.empty[Int])) {
      case (input: (Seq[String], Vector[Int]), _) => calculateResourceValue(input)
    }
  println(resourceValues)

  val repeatingEvery = 28
  val lastRepeating = resourceValues.view(1000 - repeatingEvery, 1000)
  val minutes = 1000000000 - 1000
  val relevantIndex = minutes % repeatingEvery
  val relevantResourceValue = lastRepeating(relevantIndex)
  println(relevantResourceValue)
}
