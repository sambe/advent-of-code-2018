package com.sambe.aoc2018

import java.io.{BufferedReader, InputStreamReader}

import scala.collection.mutable

/**
  * Created by samuelberner on 01.12.18.
  */
object FileUtil {

  def loadLines(name: String): Seq[String] = {
    val rs = getClass.getClassLoader.getResourceAsStream(name)
    val br = new BufferedReader(new InputStreamReader(rs))
    val lines = mutable.Buffer[String]()

    while(true) {
      val line = br.readLine()
      if (line == null)
        return lines.toVector
      lines += line
    }
    Nil
  }
}
