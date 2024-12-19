package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day19Test:
  private val input = InputData.fromString(
    """
      r, wr, b, g, bwu, rb, gb, br

      brwrr
      bggr
      gbbr
      rrbgbr
      ubwu
      bwurrg
      brgr
      bbrgwb
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day19.part1(input) == 6)

  @Test
  def part2Works(): Unit =
    assert(Day19.part2(input) == 16)
