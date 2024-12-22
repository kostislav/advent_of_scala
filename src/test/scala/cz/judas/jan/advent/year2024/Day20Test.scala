package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day20Test:
  private val input = InputData.fromString(
    """
      ###############
      #...#...#.....#
      #.#.#.#.#.###.#
      #S#...#.#.#...#
      #######.#.#.###
      #######.#.#...#
      #######.#.###.#
      ###..E#...#...#
      ###.#######.###
      #...###...#...#
      #.#####.#.###.#
      #.#...#.#.#...#
      #.#.#.#.#.#.###
      #...#...#...###
      ###############
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day20.solve(input, 2, 38) == 3)

  @Test
  def part2Works(): Unit =
    assert(Day20.solve(input, 20, 72) == 29)
