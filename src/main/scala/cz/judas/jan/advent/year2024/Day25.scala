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
        Lock(
          lockArray.numRows - 1,
          lockArray.columnIndices
            .map: column =>
              lockArray.rowIndices.filter(row => lockArray(Position(row, column)) == '#').max
        )

    val keys = keyArrays
      .map: keyArray =>
        keyArray.columnIndices
          .map: column =>
            keyArray.numRows - keyArray.rowIndices.filter(row => keyArray(Position(row, column)) == '#').min

    locks
      .map: lock =>
        keys.count: key =>
          lock.pins.zip(key).forall((lockPins, keyPins) => lockPins + keyPins <= lock.height)
      .sum

  def part2(input: InputData): Int =
    0


case class Lock(height: Int, pins: Seq[Int])
