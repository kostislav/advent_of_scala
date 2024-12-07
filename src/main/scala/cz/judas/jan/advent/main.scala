package cz.judas.jan.advent

import cz.judas.jan.advent.year2024.Day07

@main def entrypoint(): Unit =
  val startTime = System.currentTimeMillis()
  val result = run(year = 2024, day = 7, part = 2)
  val endTime = System.currentTimeMillis()
  println(result)
  println(s"It took ${endTime - startTime} ms")
