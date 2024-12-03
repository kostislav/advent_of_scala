package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.InputData

import java.util.regex.Pattern

object Day03:
  def part1(input: InputData): Int =
    val pattern = Pattern.compile("mul\\([0-9]{1,3},[0-9]{1,3}\\)")
    val pattern2 = Pattern.compile("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")
    val matcher = pattern.matcher(input.whole)
    var result = 0
    while matcher.find() do
      val matcher2 = pattern2.matcher(matcher.group(0))
      matcher2.find()
      result += matcher2.group(1).toInt * matcher2.group(2).toInt
    result

  def part2(input: InputData): Int =
    val pattern = Pattern.compile("mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)")
    val pattern2 = Pattern.compile("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")
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
          val matcher2 = pattern2.matcher(instruction)
          matcher2.find()
          result += matcher2.group(1).toInt * matcher2.group(2).toInt
    result
