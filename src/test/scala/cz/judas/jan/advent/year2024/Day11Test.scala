package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day11Test:
  private val input = InputData.fromString("125 17")

  @Test
  def part1Works(): Unit =
    assert(Bay11.part1(input) == 55312)
