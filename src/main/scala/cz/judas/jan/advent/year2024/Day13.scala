package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, blocks, pattern, solveSystem}

object Day13:
  def part1(input: InputData): Long =
    parse(input)
      .flatMap(solve)
      .sum

  def part2(input: InputData): Long =
    parse(input)
      .map(_.offsetPrize(10000000000000L))
      .flatMap(solve)
      .sum

  private def parse(input: InputData): Iterator[ClawMachine] =
    input.wholeAs[Iterator[ClawMachine] @blocks]

  private def solve(clawMachine: ClawMachine): Option[Long] =
    solveSystem(
      clawMachine.buttonA.x, clawMachine.buttonB.x, clawMachine.prizeX,
      clawMachine.buttonA.y, clawMachine.buttonB.y, clawMachine.prizeY,
    )
      .map((numPushesA, numPushesB) => 3 * numPushesA + numPushesB)

@pattern("X+{}, Y+{}")
case class Button(x: Int, y: Int)

@pattern("Button A: {}\nButton B: {}\nPrize: X={}, Y={}")
case class ClawMachine(buttonA: Button, buttonB: Button, prizeX: Long, prizeY: Long):
  def offsetPrize(by: Long): ClawMachine =
    copy(prizeX = prizeX + by, prizeY = prizeY + by)
