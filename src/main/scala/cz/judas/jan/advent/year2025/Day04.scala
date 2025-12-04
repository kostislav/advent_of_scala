package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, Position}

object Day04:
  def part1(input: InputData): Int =
    val rolls = findRolls(input)
    rolls.count(canBeLifted(_, rolls))

  def part2(input: InputData): Int =
    val rolls = findRolls(input)
    bleh(rolls)

  private def bleh(rolls: Set[Position]): Int =
    val toLift = rolls
      .filter(canBeLifted(_, rolls))

    if toLift.isEmpty then
      0
    else
      toLift.size + bleh(rolls -- toLift)

  private def findRolls(input: InputData): Set[Position] =
    input.asArray2d.positionsOf('@')

  private def canBeLifted(roll: Position, allRolls: Set[Position]): Boolean =
    roll.neighbors(includeDiagonal = true).count(allRolls.contains) < 4