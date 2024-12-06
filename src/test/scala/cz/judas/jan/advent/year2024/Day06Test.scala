package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day06Test:
  private val input = InputData.fromString(
    """
      ....#.....
      .........#
      ..........
      ..#.......
      .......#..
      ..........
      .#..^.....
      ........#.
      #.........
      ......#...
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day06.part1(input) == 41)

  @Test
  def part2Works(): Unit =
    assert(Day06.part2(input) == 6)
