package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day01Test:
  private val input = InputData.fromString(
    """
      L68
      L30
      R48
      L5
      R60
      L55
      L1
      L99
      R14
      L82
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day01.part1(input) == 3)

  @Test
  def part2Works(): Unit =
    assert(Day01.part2(input) == 6)
