package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, DirectionalPosition, InputData, Position, RelativePosition}

import scala.collection.mutable

object Day06:
  def part1(input: InputData): Int =
    val (area, startingPosition) = parseInput(input)

    getVisited(area, startingPosition).size

  def part2(input: InputData): Int =
    val (area, startingPosition) = parseInput(input)
    val candidates = getVisited(area, startingPosition)

    candidates
      .count: additionalObstaclePosition =>
        if area.get(additionalObstaclePosition).contains('.') then
          var position = startingPosition
          val visited = mutable.HashSet[DirectionalPosition]()
          var loop = false

          while area.contains(position.position) && !loop do
            if visited.contains(position) then
              loop = true
            else
              visited.add(position)
              val nextPosition = position.walk(1)
              if area.get(nextPosition.position).contains('#') || nextPosition.position == additionalObstaclePosition then
                position = position.turnRight
              else
                position = nextPosition

          loop
        else
          false

  private def parseInput(input: InputData): (Array2d, DirectionalPosition) =
    val area = input.asArray2d
    val startingPosition = area.positionOfOnly('^')

    (area, DirectionalPosition(startingPosition, RelativePosition.UP))

  private def getVisited(area: Array2d, startingPosition: DirectionalPosition): Set[Position] =
    Iterator
      .unfold(startingPosition): position =>
        if area.contains(position.position) then
          val nextPosition = position.walk(1)
          if area.get(nextPosition.position).contains('#') then
            Some((position.position, position.turnRight))
          else
            Some((position.position, nextPosition))
        else
          None
      .toSet
