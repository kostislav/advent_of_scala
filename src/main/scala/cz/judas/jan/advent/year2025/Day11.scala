package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, pattern, separatedBy}

object Day11:
  def part1(input: InputData): Long =
    val graph = input.linesAs[Device]
      .map: line =>
        line.input -> line.outputs.toSet
      .toMap

    bleh(graph, Seq("you"))

  def part2(input: InputData): Long =
    val graph = input.linesAs[Device]
      .map: line =>
        line.input -> line.outputs.toSet
      .toMap

//    println("digraph {") // TODO
//    graph.foreach: (node, neighbors) =>
//      neighbors.foreach: neighbor =>
//        println(s"${node} -> ${neighbor};") // TODO
//    println("}") // TODO

    val level1 = Set("bvg", "kvz", "szm", "ufi", "cab")
    val level1Counts = level1
      .map(node => node -> bleh2(graph, Seq("svr"), node, level1, None))
      .toMap

    println(level1Counts) // TODO

    val level2 = Set("yvk", "jqr", "jyv", "aoc")
    val level2Counts = level2
      .map: node2 =>
        node2 -> level1
          .map: node1 =>
            level1Counts(node1) * bleh2(graph, Seq(node1), node2, level2, Some("fft"))
          .sum
      .toMap

    println(level2Counts) // TODO

    val level3 = Set("xuu", "quu", "tcj", "rtv", "rue")
    val level3Counts = level3
      .map: node3 =>
        node3 -> level2
          .map: node2 =>
            level2Counts(node2) * bleh2(graph, Seq(node2), node3, level3, None)
          .sum
      .toMap

    println(level3Counts) // TODO

    val level4 = Set("dje", "gwj", "tnh")
    val level4Counts = level4
      .map: node4 =>
        node4 -> level3
          .map: node3 =>
            level3Counts(node3) * bleh2(graph, Seq(node3), node4, level4, None)
          .sum
      .toMap

    println(level4Counts) // TODO

    val level5 = Set("svb", "ecy", "you")
    val level5Counts = level5
      .map: node5 =>
        node5 -> level4
          .map: node4 =>
            level4Counts(node4) * bleh2(graph, Seq(node4), node5, level5, Some("dac"))
          .sum
      .toMap

    println(level5Counts) // TODO

    val level6 = Set("out")
    val level6Counts = level6
      .map: node6 =>
        node6 -> level5
          .map: node5 =>
            level5Counts(node5) * bleh2(graph, Seq(node5), node6, level6, None)
          .sum
      .toMap

    println(level6Counts) // TODO

    level6Counts("out")

  private def bleh(graph: Map[String, Set[String]], path: Seq[String]): Int =
    graph.getOrElse(path.last, Seq.empty).iterator
      .map: neighbor =>
        if neighbor == "out" then
          1
        else if !path.contains(neighbor) then
          bleh(graph, path ++ Seq(neighbor))
        else
          0
      .sum

  private def bleh2(graph: Map[String, Set[String]], path: Seq[String], target: String, stopAt: Set[String], mustGoThrough: Option[String]): Long =
    graph.getOrElse(path.last, Seq.empty).iterator
      .map: neighbor =>
        if neighbor == target then
          if mustGoThrough.forall(path.contains) then
            1L
          else
            0L
        else if stopAt.contains(neighbor) then
          0L
        else if !path.contains(neighbor) then
          bleh2(graph, path ++ Seq(neighbor), target, stopAt, mustGoThrough)
        else
          0L
      .sum


@pattern("{}: {}")
case class Device(input: String, outputs: Seq[String] @separatedBy(" "))