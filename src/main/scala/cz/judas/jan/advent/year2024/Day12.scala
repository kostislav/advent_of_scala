package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition}

import scala.collection.mutable

object Day12:
  def part1(input: InputData): Int =
    val garden = Array2d.fromInput(input)
    val visited = mutable.HashSet[Position]()
    garden.indices
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
        val perimeter = region.iterator.map(plot => 4 - RelativePosition.horizontalDirections.count(d => garden.get(plot + d).contains(plantType))).sum
        region.size * perimeter
      .sum

  def part2(input: InputData): Int =
    val garden = Array2d.fromInput(input)
    val visited = mutable.HashSet[Position]()
    garden.indices
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
        val numberOfSides = region.iterator
          .flatMap: plot =>
            Seq(plot, plot + RelativePosition.RIGHT, plot + RelativePosition.DOWN, plot + RelativePosition.RIGHT + RelativePosition.DOWN)
          .toSet
          .iterator
          .map: point =>
            Seq(point + RelativePosition.UP + RelativePosition.LEFT, point + RelativePosition.UP, point + RelativePosition.LEFT, point)
              .map(point => region.contains(point) && garden.get(point).contains(plantType)) match
              case Seq(false, false, false, false) => 0
              case Seq(false, false, false, true) => 1
              case Seq(false, false, true, false) => 1
              case Seq(false, false, true, true) => 0

              case Seq(false, true, false, false) => 1
              case Seq(false, true, false, true) => 0
              case Seq(false, true, true, false) => 2
              case Seq(false, true, true, true) => 1

              case Seq(true, false, false, false) => 1
              case Seq(true, false, false, true) => 2
              case Seq(true, false, true, false) => 0
              case Seq(true, false, true, true) => 1

              case Seq(true, true, false, false) => 0
              case Seq(true, true, false, true) => 1
              case Seq(true, true, true, false) => 1
              case Seq(true, true, true, true) => 0
          .sum
        region.size * numberOfSides
      .sum


