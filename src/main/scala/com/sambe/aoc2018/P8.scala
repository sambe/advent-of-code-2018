package com.sambe.aoc2018

object P8 extends App {

  val inputLines = FileUtil.loadLines("p8.in")

  val numbers = inputLines.head.split(" ").map(_.toInt)

  case class Node(subNodes: Seq[Node], metadata: Seq[Int]) {
    override def toString(): String = toString("")
    def toString(prefix: String): String = {
      prefix + "Node with metadata " + metadata.mkString("[", ", ", "]") + "(\n" +
      subNodes.map(_.toString(prefix + "  ")).mkString +
      prefix + ")\n"
    }
  }

  def parseNode(iterator: Iterator[Int]): Node = {
    val nChildren = iterator.next()
    val nMetadata = iterator.next()
    val subNodes = for (_ <- 1 to nChildren) yield parseNode(iterator)
    val metadata = for (_ <- 1 to nMetadata) yield iterator.next()
    Node(subNodes, metadata)
  }

  val tree = parseNode(numbers.iterator)
  println(tree)

  def sumMetadata(tree: Node): Int = {
    tree.metadata.sum + tree.subNodes.map(sumMetadata).sum
  }

  val metadataSum = sumMetadata(tree)
  println(s"metadata sum: $metadataSum")

  def calculateValue(tree: Node): Int = {
    if (tree.subNodes.isEmpty)
      tree.metadata.sum
    else {
      val subNodeValues = tree.subNodes.map(calculateValue)
      tree.metadata.filter(md => md >= 1 && md <= subNodeValues.size).map(md => subNodeValues(md - 1)).sum
    }
  }
  val value = calculateValue(tree)
  println(s"value: $value")
}
