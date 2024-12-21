package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, getOnlyElement, shortestPath}

import scala.annotation.tailrec

object Day21:
  def part1(input: InputData): Int =
    val numericKeypad = Array2d.fromRows("789", "456", "123", ".0A")
    val directionalKeypad = Array2d.fromRows(".^A", "<v>")

    var state = IndexedSeq(Position(3, 2), Position(0, 2), Position(0, 2))

    input.lines
      .map: line =>
        var numMoves = 0
        line.foreach: c =>
          val endState = IndexedSeq(numericKeypad.indices.filter(numericKeypad(_) == c).getOnlyElement) ++ IndexedSeq.fill(2)(Position(0, 2))
//          TODO use the other one
          numMoves += 1 + shortestPath
            (state)
            (_ == endState)
            (state =>
              Seq('^', 'v', '>', '<', 'A').flatMap(key => transition(numericKeypad, directionalKeypad, state, 2, key)).map(_ -> 1)
            )
            .get
          state = endState
        numMoves * line.replace("A", "").toInt
      .sum

  def part2(input: InputData): Int =
    0

  @tailrec
  private def transition(numericKeypad: Array2d, directionalKeypad: Array2d, state: IndexedSeq[Position], index: Int, keyPress: Char): Option[IndexedSeq[Position]] =
    def move(direction: RelativePosition): Option[IndexedSeq[Position]] =
      val newPosition = state(index) + direction
      val keypad = if index == 0 then numericKeypad else directionalKeypad
      if keypad.get(newPosition).exists(_ != '.') then
        Some(state.updated(index, newPosition))
      else
        None

    keyPress match
      case '^' => move(RelativePosition.UP)
      case '>' => move(RelativePosition.RIGHT)
      case 'v' => move(RelativePosition.DOWN)
      case '<' => move(RelativePosition.LEFT)
      case 'A' =>
        if index == 0 then
          None
        else
          transition(numericKeypad, directionalKeypad, state, index - 1, directionalKeypad(state(index)))
