package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.{InputData, histogram, pattern}

object Day04:
  def part1(input: InputData): Int =
    input.linesAs[Room]
      .filter: room =>
        val checksum = room.encryptedName
          .flatten
          .histogram
          .entries
          .sorted(using Ordering.by[(Char, Int), Int](_._2).reverse.orElseBy(_._1))
          .map(_._1)
          .take(5)
          .mkString("")
        checksum == room.checksum
      .map(_.sectorId)
      .sum

  def part2(input: InputData): Int =
    0

@pattern("{}{}[{}]")
case class Room(encryptedName: Seq[String @pattern("{}-")], sectorId: Int, checksum: String)
