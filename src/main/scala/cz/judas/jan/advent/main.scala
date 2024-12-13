package cz.judas.jan.advent

@main def entrypoint(): Unit =
  val startTime = System.currentTimeMillis()
  val result = run(year = 2024, day = 13, part = 2)
  val endTime = System.currentTimeMillis()
  println(result)
  println(s"It took ${endTime - startTime} ms")
