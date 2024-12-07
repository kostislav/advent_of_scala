package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, getOnlyElement}

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

object Day06:
  def part1(input: InputData): Int =
    val (area, startingPosition, startingDirection) = parseInput(input)

    getVisited(area, startingPosition, startingDirection).size

  def part2(input: InputData): Int =
    val (area, startingPosition, startingDirection) = parseInput(input)
    val candidates = getVisited(area, startingPosition, startingDirection)

    candidates
      .par
      .count: additionalObstaclePosition =>
        if area.get(additionalObstaclePosition).contains('.') then
          var position = startingPosition
          var direction = RelativePosition(-1, 0)
          val visited = mutable.HashSet[(Position, RelativePosition)]()
          var loop = false

          while area.contains(position) && !loop do
            if visited.contains((position, direction)) then
              loop = true
            else
              visited.add((position, direction))
              val nextPosition = position + direction
              if area.get(nextPosition).contains('#') || nextPosition == additionalObstaclePosition then
                direction = direction.rotateRight
              else
                position = nextPosition

          loop
        else
          false

  private def parseInput(input: InputData): (Array2d, Position, RelativePosition) =
    val area = Array2d.fromInput(input)
    val startingPosition = area.indices.filter(position => area.get(position).contains('^')).getOnlyElement
    val startingDirection = RelativePosition(-1, 0)

    (area, startingPosition, startingDirection)

  private def getVisited(area: Array2d, startingPosition: Position, startingDirection: RelativePosition): Set[Position] =
    Iterator
      .unfold((startingPosition, startingDirection)):
        case (position, direction) =>
          if area.contains(position) then
            val nextPosition = position + direction
            if area.get(nextPosition).contains('#') then
              Some((position, (position, direction.rotateRight)))
            else
              Some((position, (nextPosition, direction)))
          else
            None
      .toSet
