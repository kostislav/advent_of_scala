package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData

import java.util.regex.Pattern

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
    val pattern = Pattern.compile("Button A: X\\+(\\d+), Y\\+(\\d+)\nButton B: X\\+(\\d+), Y\\+(\\d+)\nPrize: X=(\\d+), Y=(\\d+)")
    input.whole.split("\n\n").iterator
      .map: clawMachineStr =>
        val matcher = pattern.matcher(clawMachineStr)
        matcher.find()
        ClawMachine(
          Button(matcher.group(1).toInt, matcher.group(2).toInt),
          Button(matcher.group(3).toInt, matcher.group(4).toInt),
          matcher.group(5).toInt,
          matcher.group(6).toInt,
        )

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

case class Button(x: Int, y: Int)

case class ClawMachine(buttonA: Button, buttonB: Button, prizeX: Long, prizeY: Long):
  def offsetPrize(by: Long): ClawMachine =
    copy(prizeX = prizeX + by, prizeY = prizeY + by)
