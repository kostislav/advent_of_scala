package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day02Test:
  private val input = InputData.fromString(
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
  )

  @Test
  def part1Works(): Unit =
    assert(Day02.part1(input) == 1227775554L)

  @Test
  def part2Works(): Unit =
    assert(Day02.part2(input) == 4174379265L)
