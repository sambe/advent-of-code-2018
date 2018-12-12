package com.sambe.aoc2018

import scala.collection.mutable

object P7 extends App {

  val inputLines = FileUtil.loadLines("p7.in")

  /*val inputLines = Seq(
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin."
  )*/

  // example line: Step Z must be finished before step V can begin.

  val dependencies = inputLines.map { l =>
    (l.charAt(5), l.charAt(l.length-12))
  }

  val depsByTarget = dependencies.groupBy(_._2).mapValues(_.map(_._1))

  val notCompleted = mutable.Set[Char]()
  notCompleted ++= dependencies.map(_._1) ++ dependencies.map(_._2)
  val completed = mutable.Set[Char]()

  val order = mutable.Buffer[Char]()
  var timeNow = 0
  var workers = 5
  var inProgress = mutable.Set[Char]()
  var toBeCompletedBy = mutable.Map[Char, Int]()

  while(notCompleted.nonEmpty) {

    // release from progress
    val completedTasks = toBeCompletedBy.filter { case (task, time) =>
        inProgress.contains(task) && timeNow >= time
    }.keys
    //println(s"$timeNow - completed: $completedTasks")
    inProgress --= completedTasks
    workers += completedTasks.size
    order ++= completedTasks
    completed ++= completedTasks
    notCompleted --= completedTasks

    val ready = (notCompleted -- inProgress).filter(nc =>
      depsByTarget.getOrElse(nc, Nil).forall(completed.contains)
    )
    val selected = if (ready.nonEmpty) ready.min else '-'
    //println(s"$timeNow - available: $ready")
    //println(s"$timeNow - $selected")

    val toTake = math.max(math.min(ready.size, workers), 0)
    var startableNow = ready.toSeq.sorted.take(toTake)
    //println(s"$timeNow - workers: $workers - starting: $startableNow")
    for (taskToStart <- startableNow) {
      workers -= 1
      val completionTime = timeNow + 60 + taskToStart-'A'+1
      inProgress += taskToStart
      toBeCompletedBy(taskToStart) = completionTime
    }

    println("%4d   %s  %s" format (timeNow, (inProgress.toSeq ++ Seq.fill(5-inProgress.size)(' ')).sorted.mkString("   "), order.mkString))

    timeNow += 1
  }
  println()
  println(order.mkString)

  println()
  println(toBeCompletedBy)
  println()
  println(toBeCompletedBy.maxBy(_._2))
}
