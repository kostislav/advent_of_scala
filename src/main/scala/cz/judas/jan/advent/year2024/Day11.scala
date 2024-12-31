package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, recurseMemoized, separatedBy}

object Day11:
  def part1(input: InputData): Long =
    solve(input, 25)

  def part2(input: InputData): Long =
    solve(input, 75)

  private def solve(input: InputData, numBlinks: Int): Long =
    input.wholeAs[Seq[Long] @separatedBy(" ")]
      .map: initialStone =>
        recurseMemoized[(Int, Long), Long]((numBlinks, initialStone)):
          case ((remainingBlinks, stone), recursion) =>
            if remainingBlinks == 0 then
              1
            else
              blink(stone)
                .map(newStone => (remainingBlinks - 1, newStone))
                .map(recursion)
                .sum
      .sum

  private def blink(stone: Long): Seq[Long] =
    if stone == 0 then
      Seq(1)
    else
      val digits = stone.toString
      if digits.length % 2 == 0 then
        val half = digits.length / 2
        Seq(
          digits.substring(0, half).toLong,
          digits.substring(half).toLong,
        )
      else
        Seq(stone * 2024)
