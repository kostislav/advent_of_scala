package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, floodFill, unique}

import scala.collection.mutable

object Day12:
  def part1(input: InputData): Int =
    val garden = input.asArray2d
    regions(garden)
      .map: region =>
        val perimeter = region.iterator
          .map(plot => RelativePosition.horizontalDirections.count(d => !region.contains(plot + d)))
          .sum
        region.size * perimeter
      .sum

  def part2(input: InputData): Int =
    val garden = input.asArray2d
    regions(garden)
      .map: region =>
        val numberOfSides = region.iterator
          .flatMap(plot => Seq(plot, plot + RelativePosition.RIGHT, plot + RelativePosition.DOWN, plot + RelativePosition.RIGHT + RelativePosition.DOWN))
          .unique
          .map(numberOfCorners(_, garden, region))
          .sum
        region.size * numberOfSides
      .sum

  private def numberOfCorners(point: Position, garden: Array2d, region: Set[Position]): Int =
    val topLeft = region.contains(point + RelativePosition.UP + RelativePosition.LEFT)
    Seq(point + RelativePosition.UP, point + RelativePosition.LEFT, point)
      .map(point => region.contains(point) == topLeft) match
      case Seq(false, false, false) => 1
      case Seq(false, false, true) => 2
      case Seq(false, true, false) => 0
      case Seq(false, true, true) => 1

      case Seq(true, false, false) => 0
      case Seq(true, false, true) => 1
      case Seq(true, true, false) => 1
      case Seq(true, true, true) => 0

  private def regions(garden: Array2d): Iterator[Set[Position]] =
    val visited = mutable.HashSet[Position]()
    garden.indices
      .iterator
      .filterNot(visited.contains)
      .map: plot =>
        val plantType = garden(plot)
        val region = floodFill(plot): x =>
          garden.neighbors(x)
            .filter: neighbor =>
              garden(neighbor) == plantType
        visited ++= region
        region
