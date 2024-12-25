package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position}

object Day25:
  def part1(input: InputData): Int =
    val (lockArrays, keyArrays) = input.whole.split("\n\n")
      .toSeq
      .map(schemaString => Array2d.fromRows(schemaString.linesIterator.toIndexedSeq))
      .partition(schema => schema(Position(0, 0)) == '#')

    val locks = lockArrays
      .map: lockArray =>
        Lock(lockArray.numRows, pins(lockArray))

    val keys = keyArrays.map(pins)

    locks
      .map: lock =>
        keys.count: key =>
          lock.pins.zip(key).forall((lockPins, keyPins) => lockPins + keyPins <= lock.height)
      .sum

  def part2(input: InputData): String =
    "It is done"

  private def pins(schema: Array2d): Seq[Int] =
    schema.columnIndices
      .map: column =>
        schema.rowIndices.count(row => schema(Position(row, column)) == '#')


case class Lock(height: Int, pins: Seq[Int])
