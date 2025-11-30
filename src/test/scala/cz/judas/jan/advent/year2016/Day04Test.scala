package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Day04Test:
  @Test
  def part1Works(): Unit =
    val input = InputData.fromString(
      """
        aaaaa-bbb-z-y-x-123[abxyz]
        a-b-c-d-e-f-g-h-987[abcde]
        not-a-real-room-404[oarel]
        totally-real-room-200[decoy]
      """
    )

    assert(Day04.part1(input) == 1514)

  @Test
  def part2Works(): Unit = {
    val decrypted = Day04.decrypt("zixmtkozy", 343)

    assert(decrypted == "encrypted")
  }
