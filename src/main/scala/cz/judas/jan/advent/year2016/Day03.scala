package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.{InputData, rightAligned}

object Day03:
  def part1(input: InputData): Int =
    input.linesAs[Seq[Int @rightAligned]]
      .count(isValidTriangle)

  def part2(input: InputData): Int =
    input.linesAs[Seq[Int @rightAligned]]
      .grouped(3)
      .flatMap(_.transpose)
      .count(isValidTriangle)

  private def isValidTriangle(sides: Seq[Int]): Boolean =
    val Seq(a, b, c) = sides.sorted
    a + b > c


