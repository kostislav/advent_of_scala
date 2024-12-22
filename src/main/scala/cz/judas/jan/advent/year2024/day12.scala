package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, unique}

import scala.collection.mutable

object Day12:
  def part1(input: InputData): Int =
    val garden = input.asArray2d
    regions(garden)
      .map: region =>
        val perimeter = region.plots.iterator
          .map(plot => 4 - RelativePosition.horizontalDirections.count(d => garden.get(plot + d).contains(region.plantType)))
          .sum
        region.size * perimeter
      .sum

  def part2(input: InputData): Int =
    val garden = input.asArray2d
    regions(garden)
      .map: region =>
        val numberOfSides = region.plots.iterator
          .flatMap(plot => Seq(plot, plot + RelativePosition.RIGHT, plot + RelativePosition.DOWN, plot + RelativePosition.RIGHT + RelativePosition.DOWN))
          .unique
          .map(numberOfCorners(_, garden, region))
          .sum
        region.size * numberOfSides
      .sum

  private def numberOfCorners(point: Position, garden: Array2d, region: Region): Int =
    val topLeft = region.plots.contains(point + RelativePosition.UP + RelativePosition.LEFT)
    Seq(point + RelativePosition.UP, point + RelativePosition.LEFT, point)
      .map(point => region.plots.contains(point) == topLeft) match
      case Seq(false, false, false) => 1
      case Seq(false, false, true) => 2
      case Seq(false, true, false) => 0
      case Seq(false, true, true) => 1

      case Seq(true, false, false) => 0
      case Seq(true, false, true) => 1
      case Seq(true, true, false) => 1
      case Seq(true, true, true) => 0

  private def regions(garden: Array2d): Iterator[Region] =
    val visited = mutable.HashSet[Position]()
    garden.indices
      .iterator
      .filterNot(visited.contains)
      .map: plot =>
        val plantType = garden(plot)
        val region = mutable.HashSet[Position]()
        val toVisit = mutable.Queue[Position](plot)
        while toVisit.nonEmpty do
          val next = toVisit.dequeue()
          if garden(next) == plantType && !visited.contains(next) then
            region += next
            visited += next
            toVisit ++= garden.neighbors(next)
        Region(plantType, region.toSet)


case class Region(plantType: Char, plots: Set[Position]):
  def size: Int =
    plots.size
