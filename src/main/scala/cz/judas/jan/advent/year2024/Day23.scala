package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{AutoMap, InputData, splitOnce, transformValues}

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


def maximumClique[T](graph: UndirectedGraph[T]): Set[T] =
  def bleh(r: Set[T], p: mutable.Set[T], x: mutable.Set[T]): Option[Set[T]] =
    if p.isEmpty && x.isEmpty then
      Some(r)
    else
      p
        .iterator
        .flatMap: v =>
          val neighbors = graph.neighborsOf(v)
          val done = bleh(r + v, mutable.HashSet.from(p).intersect(neighbors), mutable.HashSet.from(p).intersect(neighbors))
          p -= v
          x -= v
          done
        .maxByOption(_.size)

  bleh(Set.empty, mutable.HashSet.from(graph.nodes), mutable.HashSet()).getOrElse(Set.empty)


class UndirectedGraph[T](neighbors: Map[T, Set[T]]):
  def nodes: Set[T] =
    neighbors.keys.toSet

  def neighborsOf(node: T): Set[T] =
    neighbors(node)

object UndirectedGraph:
  def builder[T]: Builder[T] =
    Builder()

  class Builder[T]:
    private val neighbors = AutoMap[T, mutable.Set[T]](_ => mutable.HashSet())

    def addEdge(node1: T, node2: T): Unit =
      neighbors.getOrCreate(node1) += node2
      neighbors.getOrCreate(node2) += node1

    def build: UndirectedGraph[T] =
      UndirectedGraph(
        neighbors.toMap.transformValues(_.toSet)
      )
