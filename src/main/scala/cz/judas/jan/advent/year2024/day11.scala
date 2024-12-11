package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, recurseMemoized}

object Day11:
  def part1(input: InputData): Long =
    solve(input, 25)

  def part2(input: InputData): Long =
    solve(input, 75)

  private def solve(input: InputData, numBlinks: Int): Long =
    input.whole.split(' ').map(_.toLong)
      .map: stone =>
        recurseMemoized[State, Long](State(stone, numBlinks)): (state, recursion) =>
          if state.remainingBlinks == 0 then
            1
          else
            state.blink.map(recursion).sum
      .sum


case class State(number: Long, remainingBlinks: Int):
  def blink: Seq[State] =
    if number == 0 then
      Seq(State(1, remainingBlinks - 1))
    else
      val digits = number.toString
      if digits.length % 2 == 0 then
        val half = digits.length / 2
        Seq(
          State(digits.substring(0, half).toLong, remainingBlinks - 1),
          State(digits.substring(half).toLong, remainingBlinks - 1)
        )
      else
        Seq(State(number * 2024, remainingBlinks - 1))

