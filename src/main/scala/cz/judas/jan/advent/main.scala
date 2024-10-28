package cz.judas.jan.advent

import cz.judas.jan.advent.year2021.Day02

@main def hello(): Unit =
  val input = InputData.real(year = 2021, day = 2)
  println(Day02.part2(input))
