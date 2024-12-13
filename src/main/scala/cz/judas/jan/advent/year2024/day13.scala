package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData

import java.util.regex.Pattern

object Day13:
  def part1(input: InputData): Long =
    val pattern = Pattern.compile("Button A: X\\+(\\d+), Y\\+(\\d+)\nButton B: X\\+(\\d+), Y\\+(\\d+)\nPrize: X=(\\d+), Y=(\\d+)")
    input.whole.split("\n\n")
      .map: clawMachineStr =>
        val matcher = pattern.matcher(clawMachineStr)
        matcher.find()
        val clawMachine = ClawMachine(
          Button(matcher.group(1).toInt, matcher.group(2).toInt),
          Button(matcher.group(3).toInt, matcher.group(4).toInt),
          matcher.group(5).toInt,
          matcher.group(6).toInt,
        )
        val numPressesA = (clawMachine.prizeX * clawMachine.buttonB.y - clawMachine.prizeY * clawMachine.buttonB.x) / (clawMachine.buttonA.x * clawMachine.buttonB.y - clawMachine.buttonB.x * clawMachine.buttonA.y)
        val numPressesB = (clawMachine.prizeX - clawMachine.buttonA.x * numPressesA) / clawMachine.buttonB.x
        if clawMachine.buttonA.x * numPressesA + clawMachine.buttonB.x * numPressesB == clawMachine.prizeX && clawMachine.buttonA.y * numPressesA + clawMachine.buttonB.y * numPressesB == clawMachine.prizeY then
          3 * numPressesA + numPressesB
        else
          0
      .sum

  def part2(input: InputData): Long =
    val pattern = Pattern.compile("Button A: X\\+(\\d+), Y\\+(\\d+)\nButton B: X\\+(\\d+), Y\\+(\\d+)\nPrize: X=(\\d+), Y=(\\d+)")
    input.whole.split("\n\n")
      .map: clawMachineStr =>
        val matcher = pattern.matcher(clawMachineStr)
        matcher.find()
        val clawMachine = ClawMachine(
          Button(matcher.group(1).toInt, matcher.group(2).toInt),
          Button(matcher.group(3).toInt, matcher.group(4).toInt),
          matcher.group(5).toInt,
          matcher.group(6).toInt,
        ).offsetPrize(10000000000000L)
        val numPressesA = (clawMachine.prizeX * clawMachine.buttonB.y - clawMachine.prizeY * clawMachine.buttonB.x) / (clawMachine.buttonA.x * clawMachine.buttonB.y - clawMachine.buttonB.x * clawMachine.buttonA.y)
        val numPressesB = (clawMachine.prizeX - clawMachine.buttonA.x * numPressesA) / clawMachine.buttonB.x
        if clawMachine.buttonA.x * numPressesA + clawMachine.buttonB.x * numPressesB == clawMachine.prizeX && clawMachine.buttonA.y * numPressesA + clawMachine.buttonB.y * numPressesB == clawMachine.prizeY then
          3 * numPressesA + numPressesB
        else
          0
      .sum

case class Button(x: Int, y: Int)

case class ClawMachine(buttonA: Button, buttonB: Button, prizeX: Long, prizeY: Long):
  def offsetPrize(by: Long): ClawMachine =
    copy(prizeX = prizeX + by, prizeY = prizeY + by)
