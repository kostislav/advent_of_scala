package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day12Test:
  private val largerExample = InputData.fromString(
    """
      RRRRIICCFF
      RRRRIICCCF
      VVRRRCCFFF
      VVRCCCJFFF
      VVVVCJJCFE
      VVIVCCJJEE
      VVIIICJJEE
      MIIIIIJJEE
      MIIISIJEEE
      MMMISSJEEE
    """
  )

  @Test
  def part1Works(): Unit =
    val smallerExample = InputData.fromString(
      """
        AAAA
        BBCD
        BBCC
        EEEC
      """
    )

    assert(Day12.part1(smallerExample) == 140)
    assert(Day12.part1(largerExample) == 1930)

  @Test
  def part2Works(): Unit =
    val smallerExample1 = InputData.fromString(
      """
        EEEEE
        EXXXX
        EEEEE
        EXXXX
        EEEEE
      """
    )
    assert(Day12.part2(smallerExample1) == 236)
    assert(Day12.part2(largerExample) == 1206)
