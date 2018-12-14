package com.sambe.aoc2018

object Util {

  def timeIt[A](op: => A): A = {
    val start = System.currentTimeMillis()
    val result = op
    val end = System.currentTimeMillis()
    println(s"total time: ${(end-start) / 1000.0} seconds")
    result
  }
}
