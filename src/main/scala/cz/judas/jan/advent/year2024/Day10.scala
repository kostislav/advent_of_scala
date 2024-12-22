package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position}

object Day10:
  def part1(input: InputData): Int =
    scoreTrailheads(input): (map, startingPosition) =>
      bleh(map, startingPosition).toSet.size

  def part2(input: InputData): Int =
    scoreTrailheads(input): (map, startingPosition) =>
      bleh2(map, startingPosition, Seq(startingPosition)).toSet.size

  private def scoreTrailheads(input: InputData)(scoreFunction: (Array2d, Position) => Int): Int =
    val map = input.asArray2d
    map.indices
      .flatMap: startingPosition =>
        val height = map(startingPosition)
        if height != '0' then
          None
        else
          Some(scoreFunction(map, startingPosition))
      .sum

  private def bleh(map: Array2d, currentPosition: Position): Iterable[Position] =
    if map(currentPosition) == '9' then
      Seq(currentPosition)
    else
      uphillNeighbors(map, currentPosition)
        .flatMap(neighbor => bleh(map, neighbor))

  private def bleh2(map: Array2d, currentPosition: Position, trail: Seq[Position]): Iterable[Seq[Position]] =
    if map(currentPosition) == '9' then
      Seq(trail)
    else
      uphillNeighbors(map, currentPosition)
        .flatMap(neighbor => bleh2(map, neighbor, trail :+ neighbor))

  private def uphillNeighbors(map: Array2d, currentPosition: Position): Iterable[Position] =
    val currentHeight = map(currentPosition)
    map.neighbors(currentPosition)
      .filter(neighbor => map(neighbor) - currentHeight == 1)
