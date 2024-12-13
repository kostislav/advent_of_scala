package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, blocksOf, pattern}

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
    input.parseStructured(blocksOf[ClawMachine])

  private def solve(clawMachine: ClawMachine): Option[Long] =
    val a1 = clawMachine.buttonA.x
    val b1 = clawMachine.buttonB.x
    val c1 = clawMachine.prizeX
    val a2 = clawMachine.buttonA.y
    val b2 = clawMachine.buttonB.y
    val c2 = clawMachine.prizeY
    val x = (c1 * b2 - c2 * b1) / (a1 * b2 - b1 * a2)
    val y = (c1 - a1 * x) / b1
    if x > 0 && y > 0 && a1 * x + b1 * y == c1 && a2 * x + b2 * y == c2 then
      Some(3 * x + y)
    else
      None

@pattern("X+{}, Y+{}")
case class Button(x: Int, y: Int)

@pattern("Button A: {}\nButton B: {}\nPrize: X={}, Y={}")
case class ClawMachine(buttonA: Button, buttonB: Button, prizeX: Long, prizeY: Long):
  def offsetPrize(by: Long): ClawMachine =
    copy(prizeX = prizeX + by, prizeY = prizeY + by)
