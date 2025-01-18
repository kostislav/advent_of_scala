package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day01Test:
  @Test
  def part1Works(): Unit =
    assert(Day01.part1(InputData.fromString("R2, L3")) == 5)
    assert(Day01.part1(InputData.fromString("R2, R2, R2")) == 2)
    assert(Day01.part1(InputData.fromString("R5, L5, R5, R3")) == 12)

  @Test
  def part2Works(): Unit =
    assert(Day01.part2(InputData.fromString("R8, R4, R4, R8")) == 4)
