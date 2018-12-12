package com.sambe.aoc2018

object P6 extends App {

  val inputLines = FileUtil.loadLines("p6.in")

  val coordinates = inputLines.map { l =>
    val x::y::Nil = l.split(", ").map(_.toInt).toList
    (x, y)
  }
  /*val coordinates = Seq(
    (1, 1),
    (1, 6),
    (8, 3),
    (3, 4),
    (5, 5),
    (8, 9)
  )
  println(coordinates)*/
  println()

  val offset = 800
  val n = 2000

  val nearest = Array.fill[Int](n, n)(-100)
  val distance = Array.fill[Int](n, n)(10000)
  val totalSumOfMDs = Array.fill[Int](n,n)(0)

  coordinates.map{case (x, y) => (x+offset, y+offset)}.zipWithIndex.foreach { case ((x, y), c) =>
    for {
      i <- 0 until n
      j <- 0 until n
    } {
      val md = math.abs(i-y) + math.abs(j-x)
      if (md < distance(i)(j)) { // new nearest
        nearest(i)(j) = c
        distance(i)(j) = md
      } else if (md == distance(i)(j)) { // new at-least-two
        nearest(i)(j) = -1
      } else { // not relevant

      }
      totalSumOfMDs(i)(j) += md
    }
  }

  /*for {
    i <- 0 until n
    j <- 0 until n
  } {
    if (j == 0) println()
    print("%5d".format(distance(i)(j)))
    //print(('a' + nearest(i)(j)).toChar)
  }
  println()*/
  println()

  val c2size = nearest.flatten.groupBy(c => c).mapValues(_.size).toSeq.sortBy(-_._2)
  println(c2size)

  val infinite = (nearest.head ++ nearest.last ++ nearest.map(_.head) ++ nearest.map(_.last)).toSet
  println(infinite)

  val nonInfinite = c2size.filter{case (c, size) => !infinite.contains(c)}.toSeq.sortBy(-_._2)
  println(nonInfinite)

  val largest = nonInfinite.maxBy(_._2)
  println(largest)

  // part 2
  val region = totalSumOfMDs.map(_.count(_ < 10000)).sum
  println(region)
}
