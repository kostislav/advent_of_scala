package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, getOnlyElement, header, linearizeDag, lines, pattern, separatedBy, toMultiMap}

object Day05:
  def part1(input: InputData): Int =
    solve(input, equal = true)

  def part2(input: InputData): Int =
    solve(input, equal = false)

  private def solve(input: InputData, equal: Boolean): Int =
    val (orderingRules, updates) = input.wholeAs[
      Seq[OrderingRule] @lines @header,
      Iterator[Seq[Int] @separatedBy(",")] @lines
    ]

    updates
      .flatMap: update =>
        val numbers = update.toSet
        val filteredRules = orderingRules
          .filter(rule => numbers.contains(rule.first) && numbers.contains(rule.second))
          .map(rule => rule.second -> rule.first)
          .toMultiMap
        val first = (numbers -- filteredRules.keys).getOnlyElement

        val sorted = linearizeDag(first, filteredRules)

        if (sorted == update) == equal then
          Some(sorted(sorted.size / 2))
        else
          None
      .sum


@pattern("{}|{}")
case class OrderingRule(first: Int, second: Int)
