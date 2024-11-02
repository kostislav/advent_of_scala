package cz.judas.jan.advent.year2021

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day02Test:
  private val input = InputData.fromString(
    """
      forward 5
      down 5
      forward 8
      up 3
      down 8
      forward 2
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day02.part1(input) == 150)

  @Test
  def part2Works(): Unit =
    assert(Day02.part2(input) == 900)

