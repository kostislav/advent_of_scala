package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, RelativePosition, cartesianProduct, toMultiMap}

object Day08:
  def part1(input: InputData): Int =
    val map = Array2d.fromInput(input)
    map
      .indices
      .flatMap: position =>
        // TODO apply
        val value = map.get(position).get
        if value == '.' then
          None
        else
          Some(map.get(position).get -> position)
      .toMultiMap
      .values
      .flatMap: positions =>
        positions.cartesianProduct(onlyDifferent = true)
          .map:
            case (first, second) =>
              second + RelativePosition(second.row - first.row, second.column - first.column)
          .filter(map.contains)
      .toSet
      .size

  def part2(input: InputData): Int =
    val map = Array2d.fromInput(input)
    map
      .indices
      .flatMap: position =>
        // TODO apply
        val value = map.get(position).get
        if value == '.' then
          None
        else
          Some(map.get(position).get -> position)
      .toMultiMap
      .values
      .flatMap: positions =>
        positions.cartesianProduct(onlyDifferent = true)
          .flatMap:
            case (first, second) =>
              val diff = RelativePosition(second.row - first.row, second.column - first.column)
              Iterator.unfold(first): current =>
                if map.contains(current) then
                  Some((current, current + diff))
                else
                  None
              ++
                Iterator.unfold(second): current =>
                  if map.contains(current) then
                    Some((current, current - diff))
                  else
                    None
          .filter(map.contains)
      .toSet
      .size

