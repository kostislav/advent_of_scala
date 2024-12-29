package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, Position, RelativePosition, getOnlyElement, histogram, pattern}

import java.lang.Integer.signum

object Day14:
  def part1(input: InputData): Int =
    part1(input, 101, 103)

  def part1(input: InputData, width: Int, height: Int): Int =
    val numSteps = 100
    input.linesAs[Robot]
      .flatMap: robot =>
        val endPosition = (robot.position + robot.velocity * numSteps).mod(height, width)
        (signum(endPosition.column - width / 2), signum(endPosition.row - height / 2)) match
          case (-1, -1) => Some(1)
          case (-1, 1) => Some(2)
          case (1, -1) => Some(3)
          case (1, 1) => Some(4)
          case _ => None
      .histogram
      .asMap
      .values
      .product

  def part2(input: InputData): Int =
    val width = 101
    val height = 103

    val startingRobots = input.linesAs[Robot].toList

    (1 to width * height)
      .filter: numSteps =>
        val robots = startingRobots.map: robot =>
          (robot.position + robot.velocity * numSteps).mod(height, width)

        robots.toSet.size == robots.size
      .getOnlyElement


@pattern("p={} v={}")
case class Robot(position: Position @pattern("{},{}"), velocity: RelativePosition @pattern("{},{}"))
