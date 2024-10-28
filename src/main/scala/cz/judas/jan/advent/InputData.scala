package cz.judas.jan.advent

import java.nio.file.{Files, Paths}

class InputData(content: String):
  def lines: Iterator[String] =
    content.linesIterator


object InputData:
  def real(year: Int, day: Int): InputData =
    val content = Files.readString(Paths.get(f"../advent_of_rust/input/year$year/day$day%02d"))
    InputData(content)
