package cz.judas.jan.advent

@main def entrypoint(): Unit =
  val (result, time) = run(year = 2024, day = 25, part = 1)
  println(result)
  println(s"It took ${time} ms")
