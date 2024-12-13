package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day13Test:
  private val input = InputData.fromString(
    """
       Button A: X+94, Y+34
       Button B: X+22, Y+67
       Prize: X=8400, Y=5400

       Button A: X+26, Y+66
       Button B: X+67, Y+21
       Prize: X=12748, Y=12176

       Button A: X+17, Y+86
       Button B: X+84, Y+37
       Prize: X=7870, Y=6450

       Button A: X+69, Y+23
       Button B: X+27, Y+71
       Prize: X=18641, Y=10279
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day13.part1(input) == 480)
