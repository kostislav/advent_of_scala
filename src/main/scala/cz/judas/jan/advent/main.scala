package cz.judas.jan.advent

import cz.judas.jan.advent.year2021.Day01

@main def hello(): Unit =
  val input = InputData.real(year = 2021, day = 1)
  println(Day01.part2(input))
