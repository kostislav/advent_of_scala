package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.{InputData, histogram, pattern, rotateLowercase}

object Day04:
  def part1(input: InputData): Int =
    input.linesAs[Room]
      .filter: room =>
        val checksum = room.encryptedName
          .flatten
          .histogram
          .entriesDescending(Ordering[Char])
          .map(_._1)
          .take(5)
          .mkString("")
        checksum == room.checksum
      .map(_.sectorId)
      .sum

  def part2(input: InputData): Int =
    input.linesAs[Room]
      .find: room =>
        val decryptedRoomName = room.encryptedName.map(decrypt(_, room.sectorId))
        decryptedRoomName.mkString("").contains("north")
      .get
      .sectorId

  def decrypt(roomName: String, sectorId: Int): String =
    roomName
      .map(c => rotateLowercase(c, sectorId))
      .mkString("")

@pattern("{}{}[{}]")
case class Room(encryptedName: Seq[String @pattern("{}-")], sectorId: Int, checksum: String)
