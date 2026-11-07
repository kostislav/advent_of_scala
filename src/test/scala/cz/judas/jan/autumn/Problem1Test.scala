package cz.judas.jan.autumn

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Problem1Test:
  private val input = InputData.fromString(
    """
    A
    BBCCBBCCBBCCBBCC
    """
  )

  @Test
  def exampleWorks(): Unit =
    assert(Problem1.solve(input) == 30)

