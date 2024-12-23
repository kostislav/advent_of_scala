package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{AutoMap, InputData, UndirectedGraph, maximumClique, splitOnce}

import scala.collection.mutable

object Day23:
  def part1(input: InputData): Int =
    val graph = AutoMap[String, mutable.Set[String]](_ => mutable.HashSet())

    input.lines
      .map(_.splitOnce("-"))
      .flatMap: (first, second) =>
        val firstNeighbors =   graph.getOrCreate(first)
        val secondNeighbors = graph.getOrCreate(second)
        val sharedNeighbors = firstNeighbors.intersect(secondNeighbors)
        firstNeighbors += second
        secondNeighbors += first
        sharedNeighbors.map(neighbor => Set(first, second, neighbor))
      .count(component => component.exists(_.startsWith("t")))

  def part2(input: InputData): String =
    val graph =
      val builder = UndirectedGraph.builder[String]
      input.lines
        .map(_.splitOnce("-"))
        .foreach(builder.addEdge)
      builder.build

    maximumClique(graph)
      .toSeq
      .sorted
      .mkString(",")
