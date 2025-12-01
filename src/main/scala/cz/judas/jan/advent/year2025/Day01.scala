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
        val newDial = movement.direction match
          case Direction.Left => (current.dial - 100) % 100 - movement.amount
          case Direction.Right => current.dial + movement.amount
        (dial = floorMod(newDial, 100), clicks = current.clicks + newDial.abs / 100)
      .clicks


enum Direction:
  @pattern("L") case Left
  @pattern("R") case Right


@pattern("{}{}")
case class Movement(direction: Direction, amount: Int)