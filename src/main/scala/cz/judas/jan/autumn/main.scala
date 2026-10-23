package cz.judas.jan.autumn

import cz.judas.jan.advent.InputData

import java.nio.file.{Files, Paths}

def autumnInputData(day: Int): InputData =
  InputData.fromString(Files.readString(Paths.get(s"input/autumn/${day}")))


def run(day: Int): Unit =
  val input = autumnInputData(day)
  val problem = Class.forName(s"cz.judas.jan.autumn.Problem${day}$$")
    .getField("MODULE$")
    .get(null)
  val solveMethod = problem.getClass.getMethods.head
  val startTime = System.currentTimeMillis()
  val result = solveMethod.invoke(problem, input)
  val endTime = System.currentTimeMillis()
  val time = endTime - startTime

  println(result)
  println(s"It took ${time} ms")


@main
def entrypoint(): Unit =
  run(day = 1)
