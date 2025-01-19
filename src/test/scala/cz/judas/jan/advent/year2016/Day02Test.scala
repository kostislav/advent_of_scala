package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.{Array2d, InputData}
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day02Test:
  private val input = InputData.fromString(
    """
      ULL
      RRDDD
      LURDL
      UUUUD
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day02.part1(input) == "1985")

  @Test
  def part2Works(): Unit =
    assert(Day02.part2(input) == "5DB3")
