package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, pattern}

import scala.math.floorMod

object Day01:
  def part1(input: InputData): Int =
    input.linesAs[Movement]
      .scanLeft(50): (dial, movement) =>
        movement.direction match
          case Direction.Left => (dial - movement.amount) % 100
          case Direction.Right => (dial + movement.amount) % 100
      .count(_ == 0)

  def part2(input: InputData): Int =
    input.linesAs[Movement]
      .foldLeft(dial = 50, clicks = 0): (current, movement) =>
        val numFullRotations = movement.amount / 100
        val remainingAmount = movement.amount % 100
        val newDial = movement.direction match
          case Direction.Left =>
            (if current.dial == 0 then 100 else current.dial) - remainingAmount
          case Direction.Right =>
            current.dial + remainingAmount
        val extraClick = if newDial <= 0 || newDial >= 100 then 1 else 0
        (dial = floorMod(newDial, 100), clicks = current.clicks + numFullRotations + extraClick)
      .clicks


enum Direction:
  @pattern("L") case Left
  @pattern("R") case Right


@pattern("{}{}")
case class Movement(direction: Direction, amount: Int)