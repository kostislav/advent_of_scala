package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, pattern, toHashMap, toMultiMap}

import scala.collection.mutable

object Day05:
  def part1(input: InputData): Int =
    solve(input, equal = true)

  def part2(input: InputData): Int =
    solve(input, equal = false)

  private def solve(input: InputData, equal: Boolean): Int =
    val Array(orderingRulesString, updateString) = input.whole.split("\n\n")
    val orderingRules = InputData.fromString(orderingRulesString).linesAs[OrderingRule].toSeq

    updateString.linesIterator
      .flatMap: line =>
        val numbers = line.split(",").map(_.toInt).toSeq
        val numbersSet = numbers.toSet
        val filteredRules = orderingRules
          .filter(rule => numbersSet.contains(rule.first) && numbersSet.contains(rule.second))
          .map(rule => rule.second -> rule.first)
          .toMultiMap
        val first = (numbersSet -- filteredRules.keys).iterator.next()

        val sorted = linearizeDag(filteredRules ++ Map(first -> Seq.empty))

        if (sorted == numbers) == equal then
          Some(sorted(sorted.size / 2))
        else
          None
      .sum

  private def linearizeDag[T](graph: Map[T, Iterable[T]]): Seq[T] =
    val remaining = graph
      .view.mapValues(neighbors => mutable.HashSet.newBuilder[T].addAll(neighbors).result())
      .toHashMap
    val result = mutable.ArrayBuffer[T]()

    while remaining.nonEmpty do
      val next = remaining.filter((key, values) => values.isEmpty).iterator.next()._1
      result += next
      remaining.remove(next)
      remaining.values.foreach(neighbors => neighbors.remove(next))

    result.toSeq

@pattern("{}|{}")
case class OrderingRule(first: Int, second: Int)
