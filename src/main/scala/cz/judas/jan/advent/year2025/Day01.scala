package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, pattern}

object Day01:
  def part1(input: InputData): Int =
    input.linesAs[Movement]
      .scanLeft(50): (dial, movement) =>
        movement.direction match
          case Direction.Left => (dial - movement.amount) % 100
          case Direction.Right => (dial + movement.amount) % 100
      .count(_ == 0)

  def part2(input: InputData): Int =
    var dial = 50
    var numClicks = 0
    input.linesAs[Movement].foreach: movement =>
      movement.direction match
        case Direction.Left =>
          dial = dial - movement.amount
          while dial < 0 do
            dial += 100
            numClicks += 1
        case Direction.Right =>
          dial = dial + movement.amount
          while dial > 99 do
            dial -= 100
            numClicks += 1

    numClicks


enum Direction:
  @pattern("L") case Left
  @pattern("R") case Right


@pattern("{}{}")
case class Movement(direction: Direction, amount: Int)