package com.sambe.aoc2018

object P2 extends App {

  val inputLines = FileUtil.loadLines("p2.in")

  def has(targetLength: Int)(s: String): Boolean = {
    s.groupBy(c => c).values.exists(_.length == targetLength)
  }

  val has2 = inputLines.count(has(2))
  val has3 = inputLines.count(has(3))

  val checksum = has2 * has3

  println(s"$has2 * $has3 = $checksum")
  println()

  for( i1 <- inputLines.indices; i2 <- (i1 + 1) until inputLines.length) {
    val line1 = inputLines(i1)
    val line2 = inputLines(i2)
    val common = (line1 zip line2).filter(a => a._1 == a._2).map(_._1)
    if (common.length + 1 == line1.length)
      println(common.mkString)
  }
}


