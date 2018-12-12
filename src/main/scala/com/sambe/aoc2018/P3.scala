package com.sambe.aoc2018

object P3 extends App {

  val inputLines = FileUtil.loadLines("p3.in")

  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int)

  val claims = inputLines.map { line =>
    val replaced = line.replace("#", "").replace("@ ", "").replace(",", " ").replace(":", "").replace("x", " ")
    val splitted = replaced.split(" ")
    val p = splitted.map(_.toInt)
    Claim(p(0), p(1), p(2), p(3), p(4))
  }

  var a = Array.ofDim[Int](1000, 1000)

  claims.foreach { c =>
    for {
      i <- 0 until c.height
      j <- 0 until c.width
    } {
      a(c.top + i)(c.left + j) += 1
    }
  }

  val total = a.map(_.count(_ > 1)).sum
  println(total)
  println()

  val nonConflicted = claims.filter { c =>
    (0 until c.height).forall { i =>
      (0 until c.width).forall { j =>
        a(c.top + i)(c.left + j) == 1
      }
    }
  }
  println(nonConflicted.mkString("\n"))

}
