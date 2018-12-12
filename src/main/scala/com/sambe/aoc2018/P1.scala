package com.sambe.aoc2018

import scala.collection.mutable

object P1 extends App {

  val inputLines = FileUtil.loadLines("p1.in")

  val parsedLines = inputLines.filter(_.nonEmpty).map { line =>
    val sign = line(0) match {
      case '-' => -1
      case '+' => +1
      case unknown => sys.error(s"unknown sign: $unknown")
    }
    val number = line.substring(1).toInt
    sign * number
  }

  val resultingFrequency = parsedLines.sum
  println(resultingFrequency)
  println()

  val manyTimesParsed = Seq.fill(200)(parsedLines).flatten

  val accumulated = manyTimesParsed.scanLeft(0)((acc, change) => acc + change).drop(1)

  //println(tenParsed.take(20))
  //println(accumulated.take(20))

  val reachesTwice = accumulated.zipWithIndex.groupBy(_._1).values.filter(_.size > 1)
  val firstReachesTwice = reachesTwice.minBy(_(1)._2)

  println(reachesTwice.take(100))
  println(firstReachesTwice)
  println()

  // alternative code for part 2
  val seen = mutable.HashMap[Int, Int]()
  val found = mutable.Buffer[Int]()
  for {
    a <- accumulated
  } {
    if (seen.contains(a))
      found += a
    else
      seen.put(a, a)
  }
  println(found)
}
