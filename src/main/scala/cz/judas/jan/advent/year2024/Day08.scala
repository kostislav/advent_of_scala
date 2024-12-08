package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, cartesianProduct, toMultiMap}

object Day08:
  def part1(input: InputData): Int =
    val map = Array2d.fromInput(input)
    map
      .entries
      .flatMap: (position, value) =>
        if value == '.' then
          None
        else
          Some(value -> position)
      .toMultiMap
      .values
      .flatMap: positions =>
        positions.cartesianProduct(onlyDifferent = true)
          .map:
            case (first, second) =>
              second + (second - first)
          .filter(map.contains)
      .toSet
      .size

  def part2(input: InputData): Int =
    val map = Array2d.fromInput(input)
    map
      .entries
      .flatMap: (position, value) =>
        if value == '.' then
          None
        else
          Some(value -> position)
      .toMultiMap
      .values
      .flatMap: positions =>
        positions.cartesianProduct(onlyDifferent = true)
          .flatMap:
            case (first, second) =>
              val diff = second - first
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

