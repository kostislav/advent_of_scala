package cz.judas.jan.advent.year2021

import cz.judas.jan.advent.InputData

object Day01:
  def part1(input: InputData): Int =
    input.linesAs[Int]
      .sliding(2)
      .count { case Seq(previous, current) => current > previous }

  def part2(input: InputData): Int =
    input.linesAs[Int]
      .sliding(4)
      .count { case Seq(previous, _, _, current) => current > previous }

