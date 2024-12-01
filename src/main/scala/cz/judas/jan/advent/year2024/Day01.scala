package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData

import scala.collection.mutable
import scala.math.abs

object Day01:
  def part1(input: InputData): Int =
    val values = input.lines
      .map(line =>
        val parts = line.split("\\s+")
        (parts(0).toInt, parts(1).toInt)
      )
      .toList

    values.map(pair => pair(0)).sorted
      .zip(values.map(pair => pair(1)).sorted)
      .map((value1, value2) => abs(value2 - value1))
      .sum

  def part2(input: InputData): Int =
    val values = input.lines
      .map(line =>
        val parts = line.split("\\s+")
        (parts(0).toInt, parts(1).toInt)
      )
      .toList

    val histogram = mutable.HashMap[Int, Int]()
    values.map((_, value2) => value2).foreach(value => histogram.put(value, histogram.getOrElse(value, 0) + 1))
    values.map((value1, _) => value1 * histogram.getOrElse(value1, 0)).sum
