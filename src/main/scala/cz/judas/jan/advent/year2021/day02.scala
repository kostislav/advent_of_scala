package cz.judas.jan.advent.year2021

import cz.judas.jan.advent.InputData

object Day02:
  def part1(input: InputData): Int =
    val position = input.lines
      .map(parseCommand)
      .foldLeft(SimplePosition(0, 0)):
        case (position, Command(direction, amount)) =>
          direction match
            case Direction.Forward => position.copy(horizontalPosition = position.horizontalPosition + amount)
            case Direction.Down => position.copy(depth = position.depth + amount)
            case Direction.Up => position.copy(depth = position.depth - amount)

    position.horizontalPosition * position.depth

  def part2(input: InputData): Int =
    val position = input.lines
      .map(parseCommand)
      .foldLeft(ComplexPosition(0, 0, 0)):
        case (position, Command(direction, amount)) =>
          direction match
            case Direction.Down => position.copy(aim = position.aim + amount)
            case Direction.Up => position.copy(aim = position.aim - amount)
            case Direction.Forward => position.copy(
              horizontalPosition = position.horizontalPosition + amount,
              depth = position.depth + position.aim * amount
            )

    position.horizontalPosition * position.depth

def parseCommand(line: String): Command =
  val Array(direction, amount) = line.split(' ')
  Command(Direction.valueOf(direction.capitalize), amount.toInt)

enum Direction:
  case Up, Down, Forward

case class Command(direction: Direction, amount: Int)

case class SimplePosition(horizontalPosition: Int, depth: Int)

case class ComplexPosition(horizontalPosition: Int, depth: Int, aim: Int)
