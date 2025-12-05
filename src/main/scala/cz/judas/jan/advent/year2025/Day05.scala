package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InclusiveRange, InputData, RangeSet, header, lines}

object Day05:
  def part1(input: InputData): Long =
    val (freshRanges, ids) = parseInput(input)

    ids
      .count(id => freshRanges.exists(_.contains(id)))

  def part2(input: InputData): Long =
    val (freshRanges, _) = parseInput(input)

    RangeSet.from(freshRanges)
      .map(_.size)
      .sum

  private def parseInput(input: InputData): (Seq[InclusiveRange], Seq[Long]) =
    input.wholeAs[Seq[InclusiveRange] @lines @header, Seq[Long] @lines]

