package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, getOnlyElement}

import scala.collection.mutable

object Day06:
  def part1(input: InputData): Int =
    val area = Array2d.fromInput(input)
    var position = area.indices.filter(position => area.get(position).contains('^')).getOnlyElement
    var direction = RelativePosition(-1, 0)
    val visited = mutable.HashSet[Position]()

    while area.contains(position) do
      visited += position
      val nextPosition = position + direction
      if area.get(nextPosition).contains('#') then
        direction = turnRight(direction)
      else
        position = nextPosition

    visited.size

  def part2(input: InputData): Int =
    val area = Array2d.fromInput(input)
    val startingPosition = area.indices.filter(position => area.get(position).contains('^')).getOnlyElement

    area.indices
      .count: additionalObstace =>
        if area.get(additionalObstace).contains('.') then
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
              if area.get(nextPosition).contains('#') || nextPosition == additionalObstace then
                direction = turnRight(direction)
              else
                position = nextPosition

          loop
        else
          false


  def turnRight(direction: RelativePosition): RelativePosition =
    direction match
      case RelativePosition(-1, 0) => RelativePosition(0, 1)
      case RelativePosition(0, 1) => RelativePosition(1, 0)
      case RelativePosition(1, 0) => RelativePosition(0, -1)
      case RelativePosition(0, -1) => RelativePosition(-1, 0)

