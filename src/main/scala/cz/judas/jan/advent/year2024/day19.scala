package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, headerOf, rawLines, recurseMemoized, separatedBy, word}

object Day19:
  def part1(input: InputData): Int =
    val Input(towels, designs) = parse(input)

    designs
      .count: design =>
        recurseMemoized[Int, Boolean](0): (offset, recursion) =>
          if offset == design.length then
            true
          else
            towels
              .exists: towel =>
                design.startsWith(towel, offset) && recursion(offset + towel.length)

  def part2(input: InputData): Long =
    val Input(towels, designs) = parse(input)

    designs
      .map: design =>
        recurseMemoized[Int, Long](0): (offset, recursion) =>
          if offset == design.length then
            1L
          else
            towels
              .map: towel =>
                if design.startsWith(towel, offset) then
                  recursion(offset + towel.length)
                else
                  0L
              .sum
      .sum

  private def parse(input: InputData): Input =
    // TODO directly into case class?
    val (towels, designs) = input.parseStructured(
      headerOf[Seq[String @word] @separatedBy(", ")],
      rawLines
    )
    Input(towels, designs)


case class Input(towels: Seq[String], designs: Iterator[String])
