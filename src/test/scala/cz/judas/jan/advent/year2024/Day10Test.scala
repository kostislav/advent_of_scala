package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day10Test:
  private val input = InputData.fromString(
    """
      89010123
      78121874
      87430965
      96549874
      45678903
      32019012
      01329801
      10456732
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day10.part1(input) == 36)

  @Test
  def part2Works(): Unit =
    assert(Day10.part2(input) == 81)
