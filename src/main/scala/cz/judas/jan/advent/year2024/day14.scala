package cz.judas.jan.advent.year2024


import cz.judas.jan.advent.{InputData, Position, getOnlyElement, histogram, pattern}

import java.lang.Integer.signum
import scala.math.floorMod

object Day14:
  def part1(input: InputData): Int =
    part1(input, 101, 103)

  def part1(input: InputData, width: Int, height: Int): Int =
    val numSteps = 100
    val halfWidth = width / 2
    val halfHeight = height / 2
    val counts = input.linesAs[Robot]
      .flatMap: robot =>
        val endX = floorMod(robot.pX + numSteps * robot.vX, width)
        val endY = floorMod(robot.pY + numSteps * robot.vY, height)
        (signum(endX - halfWidth), signum(endY - halfHeight)) match
          case (-1, -1) => Some(1)
          case (-1, 1) => Some(2)
          case (1, -1) => Some(3)
          case (1, 1) => Some(34)
          case _ => None
      .histogram

    counts.asMap.values.product

  def part2(input: InputData): Int =
    val width = 101
    val height = 103

    val startingRobots = input.linesAs[Robot].toList

    (1 to width * height)
      .filter: numSteps =>
        val robots = startingRobots.map: robot =>
          Position(floorMod(robot.pY + numSteps * robot.vY, height), floorMod(robot.pX + numSteps * robot.vX, width))

        robots.toSet.size == robots.size
      .getOnlyElement

@pattern("p={},{} v={},{}")
case class Robot(pX: Int, pY: Int, vX: Int, vY: Int)
