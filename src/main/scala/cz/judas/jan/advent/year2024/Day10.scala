package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position}

object Day10:
  def part1(input: InputData): Int =
    val map = Array2d.fromInput(input)
    map.indices.flatMap(trailheadScore(map, _)).sum

  def part2(input: InputData): Int =
    val map = Array2d.fromInput(input)
    map.indices.flatMap(trailheadRating(map, _)).sum

  private def trailheadScore(map: Array2d, startingPosition: Position): Option[Int] =
    val height = map(startingPosition)
    if height != '0' then
      None
    else
      Some(bleh(map, startingPosition).toSet.size)

  private def bleh(map: Array2d, currentPosition: Position): Iterable[Position] =
    val currentHeight = map(currentPosition)
    if currentHeight == '9' then
      Seq(currentPosition)
    else
      map.neighbors(currentPosition)
        .filter(neighbor => map(neighbor) - currentHeight == 1)
        .flatMap(neighbor => bleh(map, neighbor))

  private def trailheadRating(map: Array2d, startingPosition: Position): Option[Int] =
    val height = map(startingPosition)
    if height != '0' then
      None
    else
      Some(bleh2(map, startingPosition, Seq(startingPosition)).toSet.size)

  private def bleh2(map: Array2d, currentPosition: Position, trail: Seq[Position]): Iterable[Seq[Position]] =
    val currentHeight = map(currentPosition)
    if currentHeight == '9' then
      Seq(trail)
    else
      map.neighbors(currentPosition)
        .filter(neighbor => map(neighbor) - currentHeight == 1)
        .flatMap(neighbor => bleh2(map, neighbor, trail :+ neighbor))
