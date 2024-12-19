package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, recurseMemoized}

object Day19:
  def part1(input: InputData): Int =
    val Array(towelString, designs) = input.whole.split("\n\n")
    val towels = towelString.split(", ").toSeq

    designs.split("\n")
      .count: design =>
        recurseMemoized[Int, Boolean](0): (offset, recursion) =>
          if offset == design.length then
            true
          else
            towels
              .exists: towel =>
                offset + towel.length <= design.length && design.substring(offset, offset + towel.length) == towel && recursion(offset + towel.length)

  def part2(input: InputData): Long =
    val Array(towelString, designs) = input.whole.split("\n\n")
    val towels = towelString.split(", ").toSeq

    designs.split("\n")
      .map: design =>
        recurseMemoized[Int, Long](0): (offset, recursion) =>
          if offset == design.length then
            1L
          else
            towels
              .map: towel =>
                if offset + towel.length <= design.length && design.substring(offset, offset + towel.length) == towel then
                  recursion(offset + towel.length)
                else
                  0L
              .sum
      .sum

