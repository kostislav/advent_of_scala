package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, Position, RelativePosition, pattern, shortestPath}

import scala.collection.mutable

object Day18:
  def part1(input: InputData): Int =
    part1(input, 70, 1024)

  def part1(input: InputData, size: Int, numBytes: Int): Int =
    val corruptedBytes = input.linesAs[Position @pattern("{},{}")]
      .take(numBytes)
      .toSet

    shortestPath(Position(0, 0), Position(size, size)): position =>
      RelativePosition.horizontalDirections
        .map(position + _)
        .filter(neighbor => neighbor.row >= 0 && neighbor.row <= size && neighbor.column >= 0 && neighbor.column <= size && !corruptedBytes.contains(neighbor))
        .map(_ -> 1)
    .get

  def part2(input: InputData): String =
    part2(input, 70)

  def part2(input: InputData, size: Int): String =
    val corruptedBytesIterator = input.linesAs[Position @pattern("{},{}")]

    val corruptedBytes = mutable.HashSet[Position]()
    var result: Option[Position] = None

    while corruptedBytesIterator.hasNext && result.isEmpty do
      val nextByte = corruptedBytesIterator.next()
      corruptedBytes += nextByte
      val path = shortestPath(Position(0, 0), Position(size, size)): position =>
          RelativePosition.horizontalDirections
            .map(position + _)
            .filter(neighbor => neighbor.row >= 0 && neighbor.row <= size && neighbor.column >= 0 && neighbor.column <= size && !corruptedBytes.contains(neighbor))
            .map(_ -> 1)
      if path.isEmpty then
        result = Some(nextByte)

    s"${result.get.row},${result.get.column}"
