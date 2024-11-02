package cz.judas.jan.advent

import java.nio.file.{Files, Paths}

class InputData private(content: String):
  def lines: Iterator[String] =
    content.linesIterator


object InputData:
  def real(year: Int, day: Int): InputData =
    val content = Files.readString(Paths.get(f"../advent_of_rust/input/year$year/day$day%02d"))
    InputData(content)

  def fromString(input: String): InputData =
    val lines = input.linesIterator.toSeq
    val trimmedLines = lines.slice(
      if (lines.head.isEmpty) 1 else 0,
      if (lines.last.chars().allMatch(_ == ' ')) lines.size - 1 else lines.size,
    )
    val numSpaces = trimmedLines.head.chars().takeWhile(_ == ' ').count().toInt
    InputData(trimmedLines.map(line => line.substring(numSpaces) + "\n").mkString)
