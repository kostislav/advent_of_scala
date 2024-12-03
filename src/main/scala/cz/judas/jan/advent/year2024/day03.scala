package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData

import java.util.regex.Pattern

object Day03:
  def part1(input: InputData): Int =
    val pattern = Pattern.compile("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")
    val matcher = pattern.matcher(input.whole)
    var result = 0
    while matcher.find() do
      result += matcher.group(1).toInt * matcher.group(2).toInt
    result

  def part2(input: InputData): Int =
    val pattern = Pattern.compile("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don't\\(\\)")
    val matcher = pattern.matcher(input.whole)
    var result = 0
    var enabled = true
    while matcher.find() do
      val instruction = matcher.group(0)
      if instruction.startsWith("don") then
        enabled = false
      else if instruction.startsWith("do") then
        enabled = true
      else
        if enabled then
          result += matcher.group(1).toInt * matcher.group(2).toInt
    result
