package com.sambe.aoc2018

object P14 extends App {

  val inputLines = FileUtil.loadLines("p14.in")

  val puzzleInput = inputLines.head

  // Vector is immutable and allows for efficient appending
  val initialScores = Vector(3, 7)

  def calculateRound(recipes: Vector[Int], elve1: Int, elve2: Int): (Vector[Int], Int, Int) = {
    val scoreElve1 = recipes(elve1)
    val scoreElve2 = recipes(elve2)
    val sum = scoreElve1 + scoreElve2
    val sumDigits = toDigits(sum)
    val newRecipes = recipes ++ sumDigits
    val elve1Moved = (elve1 + 1 + scoreElve1) % newRecipes.size
    val elve2Moved = (elve2 + 1 + scoreElve2) % newRecipes.size
    (newRecipes, elve1Moved, elve2Moved)
  }

  def toDigits(sum: Int): Seq[Int] = {
    sum.toString.toSeq.map(_ - '0')
  }

  val (allScores, _, _) = Util.timeIt((1 to 100000000).foldLeft((initialScores, 0, 1)){
    case ((scores, elve1, elve2), _) =>
      calculateRound(scores, elve1, elve2)
  })

  // first 50 to check correctness
  println(allScores.view(0, 50).mkString(", "))

  // part 1
  val part1InitialNRecipes = puzzleInput.toInt
  println(allScores.view(part1InitialNRecipes, part1InitialNRecipes+10).mkString(""))

  // part 2
  Util.timeIt {
    val allDigits = allScores.mkString
    val scoresToTheLeftOfPuzzleInput = allDigits.indexOf(puzzleInput)
    println(scoresToTheLeftOfPuzzleInput)
  }

  // time measured was 29.92 seconds for calculating all the rounds and 14.724 seconds for calling mkString and indexOf
}
