package com.sambe.aoc2018

import scala.collection.mutable

object P4 extends App {

  val inputLines = FileUtil.loadLines("p4.in")

  case class LogLine(minute: Int, guard: Option[Int], fallsAsleep: Boolean, wakeup: Boolean)

  val sortedInputLines = inputLines.sorted.filter(_.nonEmpty)
  println(sortedInputLines.mkString("\n"))
  println()

  val logLines = sortedInputLines.map { line =>
    val hours = line.substring(12, 14).toInt
    val minutes = line.substring(15, 17).toInt
    val timestamp = 60 * hours + minutes
    val rest = line.substring(19)
    if (rest.startsWith("Guard #"))
      LogLine(timestamp, Some(rest.substring(7).takeWhile(_ != ' ').toInt), false, false)
    else if (rest == "wakes up")
      LogLine(timestamp, None, false, true)
    else if (rest == "falls asleep")
      LogLine(timestamp, None, true, false)
    else
      sys.error("unexpected: " + rest)
  }
  println(logLines.mkString("\n"))
  println()

  var guard = -1
  var step = 0
  var minute = 0
  var asleep: Boolean = false
  var buffer: mutable.Buffer[Int] = _

  case class GuardSleep(guard: Int, sleep: Seq[Int])

  val guardSleep = mutable.Map[Int, mutable.Buffer[Int]]()

  def updateStepsUntil(nextStep: Int) = (step until nextStep).foreach(i => buffer(i) = buffer(i) + 1)

  logLines.foreach { l =>
    l.guard.map { g =>
      if (asleep) {
        updateStepsUntil(if(l.minute < step) 60 else l.minute)
        asleep = false
      }
      guard = g
      step = if (l.minute >= 60) 0 else l.minute
      if (!guardSleep.contains(guard))
        guardSleep(guard) = mutable.Buffer[Int](Seq.fill(60)(0):_*)
      buffer = guardSleep(guard)
      ()
    }.getOrElse {
      if (l.fallsAsleep) {
        asleep = true
        step = l.minute // move step without updating
      } else if (l.wakeup) {
        updateStepsUntil(l.minute)
        asleep = false
        step = l.minute
      } else sys.error("undefined")
    }
  }
  if (asleep)
    updateStepsUntil(60)

  println(guardSleep.mkString("\n"))
  println()

  val mostMinutes = guardSleep.maxBy(_._2.sum)
  println(mostMinutes)
  val guardId = mostMinutes._1
  val mostTimesMinute = mostMinutes._2.zipWithIndex.maxBy(_._1)._2
  println(s"$guardId * $mostTimesMinute = ${guardId * mostTimesMinute}")
  println()

  val mostTimesSameMinute = guardSleep.maxBy(_._2.max)
  println(mostTimesSameMinute)
  val mtsmGuardId = mostTimesSameMinute._1
  val mtsmMinute = mostTimesSameMinute._2.zipWithIndex.maxBy(_._1)._2
  println(s"$mtsmGuardId * $mtsmMinute = ${mtsmGuardId * mtsmMinute}")

  /*guardSleep += GuardSleep(guard, sleep)

  guardSleep.groupBy(_.guard).mapValues(_.map(_.sleep).sum)

  val events = logLines.map { l =>
    l.guard.map { g =>
      guard = g
      l
    }.getOrElse {
      l.copy(guard = Some(guard))
    }
  }.filter(l => l.wakeup || l.fallsAsleep)

  case class SleepInterval(guard: Int, from: Int, to: Int)

  assert(events.size % 2 == 0)
  val sleepIntervals = for (i <- 0 until events.size / 2) yield {
    assert(events(i).fallsAsleep)
    assert(events(i+1).wakeup)
    SleepInterval(events(i).guard.getOrElse(-1), events(i).minute, events(i + 1).minute)
  }
  val sleepIntervalsByGuard = sleepIntervals.groupBy(_.guard).mapValues(_.map(si => si.to - si.from).sum)

  val longestSleepingGuard = sleepIntervalsByGuard.maxBy(_._2)
  println(longestSleepingGuard)*/
}
