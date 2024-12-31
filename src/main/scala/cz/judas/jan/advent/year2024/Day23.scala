package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, UndirectedGraph, into, maximumClique, pattern, unique}

object Day23:
  def part1(input: InputData): Int =
    val graph = parse(input)

    graph.edges
      .flatMap: (first, second) =>
        graph.neighborsOf(first).intersect(graph.neighborsOf(second))
          .map(neighbor => Set(first, second, neighbor))
      .unique
      .count(component => component.exists(_.startsWith("t")))

  def part2(input: InputData): String =
    val graph = parse(input)

    maximumClique(graph)
      .toSeq
      .sorted
      .mkString(",")

  private def parse(input: InputData): UndirectedGraph[String] =
      input.linesAs[(String, String) @pattern("{}-{}")]  // TODO separatedBy
        .into(UndirectedGraph.fromEdges)
