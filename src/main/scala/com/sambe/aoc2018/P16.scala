package com.sambe.aoc2018

object P16 extends App {

  val inputLines = FileUtil.loadLines("p16.in")

  case class Sample(before: Seq[Int], instruction: Int, a: Int, b: Int, c: Int, after: Seq[Int])

  val sampleLineGroups = inputLines.grouped(4).takeWhile(_(0).nonEmpty)
  val samples = sampleLineGroups.map { lg =>
    def parseList(s: String) = s.trim.stripPrefix("[").stripSuffix("]").split(", ").map(_.toInt).toSeq
    val before = parseList(lg(0).stripPrefix("Before:"))
    val instructions = lg(1).split(" ").map(_.toInt).toSeq
    val after = parseList(lg(2).stripPrefix("After:"))
    Sample(before, instructions(0), instructions(1), instructions(2), instructions(3), after)
  }.toSeq

  case class Op(name: String, op: (Seq[Int], Int, Int, Int) => Seq[Int])

  val ops = Seq(
    Op("addr", (before, a, b, c) => before.updated(c, before(a) + before(b))),
    Op("addi", (before, a, b, c) => before.updated(c, before(a) + b)),
    Op("mulr", (before, a, b, c) => before.updated(c, before(a) * before(b))),
    Op("muli", (before, a, b, c) => before.updated(c, before(a) * b)),
    Op("banr", (before, a, b, c) => before.updated(c, before(a) & before(b))),
    Op("bani", (before, a, b, c) => before.updated(c, before(a) & b)),
    Op("borr", (before, a, b, c) => before.updated(c, before(a) | before(b))),
    Op("bori", (before, a, b, c) => before.updated(c, before(a) | b)),
    Op("setr", (before, a, b, c) => before.updated(c, before(a))),
    Op("seti", (before, a, b, c) => before.updated(c, a)),
    Op("gtir", (before, a, b, c) => before.updated(c, if (a > before(b)) 1 else 0)),
    Op("gtri", (before, a, b, c) => before.updated(c, if (before(a) > b) 1 else 0)),
    Op("gtrr", (before, a, b, c) => before.updated(c, if (before(a) > before(b)) 1 else 0)),
    Op("eqir", (before, a, b, c) => before.updated(c, if (a == before(b)) 1 else 0)),
    Op("eqri", (before, a, b, c) => before.updated(c, if (before(a) == b) 1 else 0)),
    Op("eqrr", (before, a, b, c) => before.updated(c, if (before(a) == before(b)) 1 else 0))
  )

  val possibleOps = samples.map { s =>
    s.instruction -> ops.filter(_.op(s.before, s.a, s.b, s.c) == s.after).map(_.name)
  }.toList

  val solutionPart1 = possibleOps.count(_._2.size >= 3)
  println(solutionPart1)
  println()

  // created manually through repeatedly running the program and the two commented out lines below
  val knownMappings = Map(
    11 -> "eqrr",
    7 -> "eqri",
    13 -> "eqir",
    12 -> "gtri",
    3 -> "gtrr",
    0 -> "gtir",
    6 -> "banr",
    14 -> "setr",
    8 -> "bani",
    2 -> "seti",
    9 -> "addr",
    5 -> "borr",
    4 -> "bori",
    15 -> "muli",
    10 -> "addi",
    1 -> "mulr"
  )

  //val opcodeNameMapping = possibleOps.groupBy(_._1).mapValues(_.map(_._2).map(_.filterNot(name => knownMappings.values.exists(_ == name))).reduce((a, b) => a.toSet.intersect(b.toSet).toSeq))
  //println(opcodeNameMapping.mkString("\n"))

  val opByOpcode = knownMappings.andThen(ops.groupBy(_.name).mapValues(_.head))

  case class Instruction(op: Op, a: Int, b: Int, c: Int) {
    def execute(state: Seq[Int]): Seq[Int] = {
      op.op(state, a, b, c)
    }
  }
  val programLines = inputLines.drop(samples.size * 4)
  val programInstructions = programLines.filter(_.nonEmpty).map { line =>
    val numbers = line.split(" ").map(_.toInt)
    Instruction(opByOpcode(numbers(0)), numbers(1), numbers(2), numbers(3))
  }


  def run(program: Seq[Instruction], initialState: Seq[Int]): Seq[Int] = {
    program.foldLeft(initialState)((state, instruction) => instruction.execute(state))
  }

  val initialState = Seq(0, 0, 0, 0)
  val finalState = run(programInstructions, initialState)

  println()
  println(finalState)
  println()
}
