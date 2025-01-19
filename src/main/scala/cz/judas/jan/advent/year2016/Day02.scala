package cz.judas.jan.advent.year2016

import cz.judas.jan.advent.{Array2d, InputData, RelativePosition, pattern, separatedBy}

object Day02:
  def part1(input: InputData): String =
    solve(
      input,
      Array2d.fromRows(
        "123",
        "456",
        "789"
      )
    )

  def part2(input: InputData): String =
    solve(
      input,
      Array2d.fromRows(
        "  1  ",
        " 234 ",
        "56789",
        " ABC ",
        "  D  ",
      ).ignoring(' ')
    )

  private def solve(input: InputData, keypad: Array2d): String =
    input.linesAs[Seq[Direction] @separatedBy("")]
      .scanLeft(keypad.positionOfOnly('5')): (startingPosition, instructions) =>
        instructions.foldLeft(startingPosition): (position, instruction) =>
          val next = position + instruction.asRelativePosition
          if keypad.contains(next) then
            next
          else
            position
      .drop(1)
      .map(keypad.apply)
      .mkString("")


enum Direction:
  @pattern("U") case Up
  @pattern("D") case Down
  @pattern("L") case Left
  @pattern("R") case Right

  def asRelativePosition: RelativePosition =
    this match
      case Direction.Up => RelativePosition.UP
      case Direction.Down => RelativePosition.DOWN
      case Direction.Left => RelativePosition.LEFT
      case Direction.Right => RelativePosition.RIGHT
