package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day09Test:
  private val input = InputData.fromString("2333133121414131402")

  @Test
  def part1Works(): Unit =
    assert(Day09.part1(input) == 1928)

  @Test
  def part2Works(): Unit =
    assert(Day09.part2(input) == 2858)
