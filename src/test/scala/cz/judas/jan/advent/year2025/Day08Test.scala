package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day08Test:
  private val input = InputData.fromString(
    """
      162,817,812
      57,618,57
      906,360,560
      592,479,940
      352,342,300
      466,668,158
      542,29,236
      431,825,988
      739,650,466
      52,470,668
      216,146,977
      819,987,18
      117,168,530
      805,96,715
      346,949,466
      970,615,88
      941,993,340
      862,61,35
      984,92,344
      425,690,689
    """
  )

  @Test
  def part1Works(): Unit =
    assert(Day08.part1(input, 10) == 40L)

  @Test
  def part2Works(): Unit =
    assert(Day08.part2(input) == 25272L)
