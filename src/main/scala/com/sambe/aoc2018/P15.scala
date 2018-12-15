package com.sambe.aoc2018

import scala.collection.mutable

object P15 extends App {

  val inputLines = FileUtil.loadLines("p15.in")

  /*val inputLines =
    """#######
      |#.G...#
      |#...EG#
      |#.#.#G#
      |#..G#E#
      |#.....#
      |#######
    """.stripMargin.split("\n").dropRight(1)*/

  val map = inputLines.map(_.toBuffer).toBuffer
  val alive = mutable.Set[Int]()

  def reinitializeMap(): Unit = {
    map.clear()
    inputLines.map(_.toBuffer).copyToBuffer(map)
  }

  case class BattleUnit(id: Int, var position: (Int, Int), var hp: Int, elve: Boolean, var hasTargets: Boolean) // attackPower = 3 constantly

  //part1()
  part2()
  //calculateNoSingleElveKilled(29)

  def part1(elvePower: Int = 3) {
    val unitsInitially = for {
      i <- 0 until map.size
      j <- 0 until map(i).size
      cell = map(i)(j)
      if cell == 'G' || cell == 'E'
    } yield {
      val id = i * map.size + j
      BattleUnit(id, (i, j), 200, cell == 'E', true)
    }
    val unitsById = unitsInitially.groupBy(_.id).mapValues(_.head)
    alive.clear()
    alive ++= unitsInitially.map(_.id)

    def hasUnitType(elve: Boolean) = unitsInitially.filter(_.elve == elve).exists(u => alive.contains(u.id))

    def hasElves = hasUnitType(true)

    def hasGobblins = hasUnitType(false)

    var round = 0
    while (hasElves && hasGobblins && unitsInitially.filter(u => alive.contains(u.id)).exists(_.hasTargets)) {
      calculateRound(unitsInitially, elvePower)
      println(s"round $round - ${alive.size} alive, ${unitsInitially.filter(u => alive.contains(u.id)).count(_.hasTargets)} with targets")
      println(map.map(_.mkString).mkString("\n"))
      round += 1
    }
    println(s"battle terminated - ${alive.size} alive, ${unitsInitially.count(_.hasTargets)} with targets")
    println(s"alive: $alive")
    val winningUnits = unitsInitially.filter(u => alive.contains(u.id))
    val totalSumOfHitPoints = winningUnits.map(_.hp).sum
    println(s"totalSumOfHps = $totalSumOfHitPoints")
    println(winningUnits.mkString("\n"))

    val outcome = (round - 1) * totalSumOfHitPoints
    println(s"outcome = $outcome")
  }

  def part2(): Unit = {
    var min = 4
    var max = 200
    while( min != max) {
      val m = min + (max-min)/2
      val probeResult = calculateNoSingleElveKilled(m)
      val result = if(probeResult) "win" else "don't win"
      println(s"With power $m elves $result.")
      if (probeResult)
        max = m
      else
        min = m+1
    }
    println(s"Conclusion: With power $min elves win.")

    reinitializeMap()
    part1(elvePower = min)
  }

  def calculateNoSingleElveKilled(elvePower: Int): Boolean = {
    reinitializeMap()
    val unitsInitially = for {
      i <- 0 until map.size
      j <- 0 until map(i).size
      cell = map(i)(j)
      if cell == 'G' || cell == 'E'
    } yield {
      val id = i * map.size + j
      BattleUnit(id, (i, j), 200, cell == 'E', true)
    }
    alive.clear()
    alive ++= unitsInitially.map(_.id)

    def countUnitsByType(elve: Boolean) = unitsInitially.filter(_.elve == elve).count(u => alive.contains(u.id))

    val initialElvesCount = unitsInitially.count(_.elve)
    def hasAllElves = countUnitsByType(true) == initialElvesCount

    def hasGobblins = countUnitsByType(false) > 0

    var round = 0
    while(hasAllElves && hasGobblins && unitsInitially.filter(u => alive.contains(u.id)).exists(_.hasTargets)) {
      calculateRound(unitsInitially, elvePower)
      //println(s"round $round - ${alive.size} alive, ${unitsInitially.filter(u => alive.contains(u.id)).count(_.hasTargets)} with targets")
      //println(map.map(_.mkString).mkString("\n"))
      round += 1
    }

    hasAllElves
  }

  def calculateRound(units: Seq[BattleUnit], elvePower: Int = 3): Unit = {
    // order of units according to starting position
    val sortedByOrder = units.sortBy(_.position)
    sortedByOrder.foreach { unit =>
      val attackPower = if(unit.elve) elvePower else 3
      if (alive.contains(unit.id))
        calculateTurn(unit, sortedByOrder, attackPower)
    }
  }

  def calculateTurn(unit: BattleUnit, sortedUnits: Seq[BattleUnit], attackPower: Int): Unit = {
    // squares in range of targets
    val sortedTargets = sortedUnits
      .filter(u => alive.contains(u.id) && u.elve != unit.elve)
    val squaresInRangeOfTargets =
      sortedTargets.map(_.position).flatMap(adjacent)
        .filter { case (i, j) => map(i)(j) == '.' || (i, j) == unit.position }

    // if it cannot already attack, it moves first
    if (!squaresInRangeOfTargets.contains(unit.position)) {
      // choose the one reachable with the fewest steps (ties by reading order)
      val nearestTarget = findNearest(unit.position, squaresInRangeOfTargets)(movable)

      // choose step which is on the shortest path (again ties by reading order)
      val nextStep = nearestTarget.flatMap(t => findNearest(t, movable(unit.position))(movable(_).reverse))

      // move, if possible
      nextStep.foreach { newPosition =>
        //println(s"${unit.id} moves from ${unit.position} to $newPosition")
        // update map
        map(newPosition._1)(newPosition._2) = if (unit.elve) 'E' else 'G'
        map(unit.position._1)(unit.position._2) = '.'
        // update unit
        unit.position = newPosition
      }
      unit.hasTargets = nextStep.nonEmpty
    }

    // if in range of target, attack:
    if(squaresInRangeOfTargets.contains(unit.position)) {
      // choose adjacent target with lowest HP (again ties by reading order)
      val inRange = adjacent(unit.position)
      val attackable = sortedTargets.filter(t => inRange.contains(t.position)) // first that is adjacent
      val toAttack = Some(attackable).filter(_.nonEmpty).map(_.minBy(_.hp))

      toAttack.foreach { victim =>
        victim.hp -= attackPower
        if (victim.hp <= 0) {
          alive -= victim.id
          map(victim.position._1)(victim.position._2) = '.'
        }
        //val attacker = if (unit.elve) "Elve" else "Gobblin"
        //println(s"$attacker ${unit.id} attacks ${victim.id} with power $attackPower, new hp=${victim.hp}, attackable from here are ${attackable.map(_.id)}")
      }
    }
  }

  def findNearest(start: (Int, Int), targets: Seq[(Int, Int)])(moves: ((Int, Int)) => Seq[(Int, Int)]): Option[(Int, Int)] = {
    if (targets.isEmpty) None else {
      val distance = breadthFirstSearch(start)(moves)
      val minReachable = targets.minBy(p => distance(p._1)(p._2))
      if (distance(minReachable._1)(minReachable._2) < 999999) Some(minReachable) else None
    }
  }

  def breadthFirstSearch(start: (Int, Int))(moves: ((Int, Int)) => Seq[(Int, Int)]): Seq[Seq[Int]] = {
    val unseen = 999999
    val a = Array.fill(map.size, map.head.size)(unseen)
    val queue = mutable.Queue[(Int, Int)]()
    queue.enqueue(start)
    a(start._1)(start._2) = 0
    while(queue.nonEmpty) {
      val p = queue.dequeue()
      val d = a(p._1)(p._2) + 1
      moves(p).foreach { case (i, j) =>
        if (d < a(i)(j)) {
          a(i)(j) = d
          queue.enqueue((i, j))
        }
      }
    }
    a.map(_.toSeq).toSeq
  }

  def movable(p: (Int, Int)): Seq[(Int, Int)] = {
    adjacent(p).filter(p => map(p._1)(p._2) == '.')
  }

  def adjacent(p: (Int, Int)): Seq[(Int, Int)] = {
    Seq((p._1-1, p._2), (p._1, p._2-1), (p._1, p._2+1), (p._1+1, p._2)).filter(onMap)
  }

  def onMap(p: (Int, Int)): Boolean = {
    p._1 >= 0 && p._1 < map.size && p._2 >= 0 && p._2 < map.size
  }
}
