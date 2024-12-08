package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, cartesianProduct, toMultiMap}

object Day08:
  def part1(input: InputData): Int =
    val map = Array2d.fromInput(input)
    antennasByType(map)
      .flatMap: positions =>
        positions.cartesianProduct(onlyDifferent = true)
          .map((first, second) => second + (second - first))
          .filter(map.contains)
      .toSet
      .size

  def part2(input: InputData): Int =
    val map = Array2d.fromInput(input)
    antennasByType(map)
      .flatMap: positions =>
        positions.cartesianProduct(onlyDifferent = true)
          .flatMap: (first, second) =>
            val diff = second - first
            multiples(map, first, diff) ++ multiples(map, second, -diff)
          .filter(map.contains)
      .toSet
      .size

  private def antennasByType(map: Array2d): Iterable[Seq[Position]] =
    map
      .entries
      .flatMap: (position, value) =>
        if value == '.' then
          None
        else
          Some(value -> position)
      .toMultiMap
      .values

  private def multiples(map: Array2d, start: Position, increment: RelativePosition): Iterator[Position] =
    Iterator.unfold(start): current =>
      if map.contains(current) then
        Some((current, current + increment))
      else
        None
