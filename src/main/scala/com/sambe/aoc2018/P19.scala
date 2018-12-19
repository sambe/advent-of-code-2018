package com.sambe.aoc2018

import scala.collection.mutable

object P19 extends App {

  val inputLines = FileUtil.loadLines("p19.in")

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

  val opByName = ops.groupBy(_.name).mapValues(_.head)

  case class Instruction(op: Op, a: Int, b: Int, c: Int) {
    def execute(state: Seq[Int]): Seq[Int] = {
      op.op(state, a, b, c)
    }
  }
  val ipRegister = inputLines.head.stripPrefix("#ip ").toInt
  val programLines = inputLines.tail
  val programInstructions = programLines.filter(_.nonEmpty).map { line =>
    val parts = line.split(" ")
    Instruction(opByName(parts(0)), parts(1).toInt, parts(2).toInt, parts(3).toInt)
  }


  def run(program: Seq[Instruction], initialState: Seq[Int]): Seq[Int] = {
    var state = initialState //mutable.Buffer[Int](initialState:_*)
    while(state(ipRegister) >= 0 && state(ipRegister) < program.size) {
      val instruction = program(state(ipRegister))
      state = instruction.execute(state)
      state = state.updated(ipRegister, state(ipRegister) + 1)
      //println(state)
    }
    state
  }

  val initialState1 = Seq.fill(6)(0)
  val finalState1 = run(programInstructions, initialState1)

  println()
  println(s"Part1: $finalState1")
  println()

  /*val initialState2 = Seq.fill(6)(0).updated(0, 1)
  val finalState2 = run(programInstructions, initialState2)

  println()
  println(s"Part2: $finalState2")
  println()*/

  val r2 = 10550400L

  val maxDiv = math.sqrt(r2).toInt

  val divisible = for (div <- 1L to maxDiv if r2 % div == 0L) yield {
    div
  }

  println(divisible.view(0, 100).mkString(", "))

  val r0 = divisible.sum + divisible.map(r2 / _).sum
  println(r0) // first was 60471, but said it is too low, with reverse divisible it is 39967680, but now too high
}
