package cz.judas.jan.autumn

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test

class Problem0Test:
  private val input = InputData.fromString(
    """
    the quick brown fox jumps over the lazy dog
    quic
    sover
    dog
    deer
    """
  )

  @Test
  def exampleWorks(): Unit =
    assert(Problem0.solve(input) == 12)


