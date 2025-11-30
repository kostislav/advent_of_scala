package cz.judas.jan.advent

@main def entrypoint(): Unit =
  val (result, time) = run(year = 2016, day = 5, part = 2)
  println(result)
  println(s"It took ${time} ms")