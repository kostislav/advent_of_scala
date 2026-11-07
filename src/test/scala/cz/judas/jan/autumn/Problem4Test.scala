package cz.judas.jan.autumn

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Problem4Test:
  private val input = InputData.fromString(
    """
    ..X..
    ...*.
    .....
    .#..#
    ....T
    """
  )

  @Test
  def exampleWorks(): Unit =
    assert(Problem4.solve(input) == 7)

