package cz.judas.jan.advent

@main def entrypoint(): Unit =
  val (result, time) = run(year = 2025, day = 1, part = 2)
  println(result)
  println(s"It took ${time} ms")