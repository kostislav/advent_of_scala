package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day03Test:
  @Test
  def part1Works(): Unit =
    val input = InputData.fromString("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

    assert(Day03.part1(input) == 161)

  @Test
  def part2Works(): Unit =
    val input = InputData.fromString("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

    assert(Day03.part2(input) == 48)
