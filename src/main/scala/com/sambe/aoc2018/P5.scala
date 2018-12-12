package com.sambe.aoc2018

object P5 extends App {

  val inputLines = FileUtil.loadLines("p5.in")

  val polymer = inputLines.head

  def reduceAllTypesOnce(polymer: String): String = {
    ('a' to 'z').foldLeft(polymer)((p, c) => reduceOnce(p, c.toString))
  }

  def reduceOnce(p: String, ch: String): String = {
    val p2 = p.replace(ch.toLowerCase() + ch.toUpperCase, "")
    val p3 = p2.replace(ch.toUpperCase() + ch.toLowerCase(), "")
    p3
  }

  def reduceNTimes(n: Int, polymer: String, log: Boolean = true, logLast5: Boolean = false) = {
    (1 to n).foldLeft((polymer, true)) { case ((p, changing), i) =>
      val reduced = if (changing) reduceAllTypesOnce(p) else p
      if (log || logLast5 && i >= n-5) println(s"$i (size = ${reduced.size}) : ${reduced.substring(0, 100)}")
      (reduced, reduced.length != p.length)
    }._1
  }

  val reducedPolymer = reduceNTimes(400, polymer)

  val stats = reducedPolymer.toLowerCase.groupBy(c => c)
  println(stats.toSeq.sortBy(_._2.length)map{
    case (c, v) => println(c + "=" + v.length)
  })

  ('a' to 'z').foreach { char =>
    val afterCompleteReduction = reducedPolymer.replace(char.toString, "").replace(char.toString.toUpperCase, "")

    val reducedPolymer2 = reduceNTimes(2000, afterCompleteReduction, log = false, logLast5 = true)
    println(s"$char (size = ${reducedPolymer2.size}) : ${reducedPolymer2.substring(0, 100)}")
  }
}
