package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{Array2d, InputData, Position}

object Day04:
  def part1(input: InputData): Int =
    val grid = input.asArray2d
    grid.indices
      .count: position =>
        grid(position) == '@' && grid.neighbors(position, includeDiagonal = true).count(neighbor => grid(neighbor) == '@') < 4

  def part2(input: InputData): Int =
    val grid = input.asArray2d
    bleh(grid, grid.indices.filter(grid(_) == '@').toSet)

  private def bleh(grid: Array2d, rolls: Set[Position]): Int =
    val canBeRemoved = rolls
      .filter: position =>
        grid.neighbors(position, includeDiagonal = true).count(rolls.contains) < 4

    if canBeRemoved.isEmpty then
      0
    else
      canBeRemoved.size + bleh(grid, rolls -- canBeRemoved)
