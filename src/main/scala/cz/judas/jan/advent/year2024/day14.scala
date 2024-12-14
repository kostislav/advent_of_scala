package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, Position, histogram, pattern}

import scala.collection.mutable
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
        if endX < halfWidth then
          if endY < halfHeight then
            Some(1)
          else if endY > halfHeight then
            Some(2)
          else
            None
        else if endX > halfWidth then
          if endY < halfHeight then
            Some(3)
          else if endY > halfHeight then
            Some(4)
          else
            None
        else
          None
      .histogram

    counts.get(1) * counts.get(2) * counts.get(3) * counts.get(4)

  def part2(input: InputData): Int =
    val width = 101
    val height = 103
    val halfWidth = width / 2
    val halfHeight = height / 2

    val startingRobots = input.linesAs[Robot].toList

    (1 to width * height).foreach: numSteps =>
      val robots = startingRobots.map: robot =>
        Position(floorMod(robot.pY + numSteps * robot.vY, height), floorMod(robot.pX + numSteps * robot.vX, width))

      val image = mutable.ArrayBuffer.fill(width * height)('.')
      robots.foreach: robot =>
        image(robot.row * width + robot.column) = '*'
      if (0 until width).forall(x => image(x) == '.') then
        println(numSteps)
        (0 until height).foreach: y =>
          (0 until width).foreach: x =>
            print(image(y * width + x))
          println()

    0

@pattern("p={},{} v={},{}")
case class Robot(pX: Int, pY: Int, vX: Int, vY: Int)
