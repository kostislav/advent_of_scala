package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{Array2d, InputData, Position}

object Day04:
  def part1(input: InputData): Int =
    val grid = input.asArray2d
    grid.indices
      .count: position =>
        grid(position) == '@' && grid.neighbors(position, includeDiagonal = true).count(neighbor => grid(neighbor) == '@') < 4

  def part2(input: InputData): Int =
    bleh(input.asArray2d, Set.empty)

  private def bleh(grid: Array2d, ignored: Set[Position]): Int =
    val canBeRemoved = grid.indices
      .filter: position =>
        !ignored.contains(position) && grid(position) == '@' && grid.neighbors(position, includeDiagonal = true).filter(neighbor => !ignored.contains(neighbor)).count(neighbor => grid(neighbor) == '@') < 4
      .toSet
    if canBeRemoved.isEmpty then
      0
    else
      canBeRemoved.size + bleh(grid, ignored ++ canBeRemoved)
