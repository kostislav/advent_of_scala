package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, getOnlyElement, linearizeDag, pattern, toMultiMap}

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
        val first = (numbersSet -- filteredRules.keys).getOnlyElement

        val sorted = linearizeDag(first, filteredRules)

        if (sorted == numbers) == equal then
          Some(sorted(sorted.size / 2))
        else
          None
      .sum

@pattern("{}|{}")
case class OrderingRule(first: Int, second: Int)
