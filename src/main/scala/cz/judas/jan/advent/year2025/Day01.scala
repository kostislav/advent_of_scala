package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, Turn, pattern}

import scala.math.floorMod

object Day01:
  def part1(input: InputData): Int =
    input.linesAs[Movement]
      .scanLeft(50): (dial, movement) =>
        movement.direction match
          case Turn.Left => (dial - movement.amount) % 100
          case Turn.Right => (dial + movement.amount) % 100
      .count(_ == 0)

  def part2(input: InputData): Int =
    input.linesAs[Movement]
      .foldLeft(dial = 50, clicks = 0): (current, movement) =>
        val newDial = movement.direction match
          case Turn.Left => (current.dial - 100) % 100 - movement.amount
          case Turn.Right => current.dial + movement.amount
        (dial = floorMod(newDial, 100), clicks = current.clicks + newDial.abs / 100)
      .clicks


@pattern("{}{}")
case class Movement(direction: Turn, amount: Int)