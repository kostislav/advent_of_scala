package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day06Test:
  private val input = InputData.fromString(
    """
      eedadn
      drvtee
      eandsr
      raavrd
      atevrs
      tsrnev
      sdttsa
      rasrtv
      nssdts
      ntnada
      svetve
      tesnvt
      vntsnd
      vrdear
      dvrsen
      enarar
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day06.part1(input) == "easter")

  @Test
  def part2Works(): Unit =
    assert(Day06.part2(input) == "advent")
