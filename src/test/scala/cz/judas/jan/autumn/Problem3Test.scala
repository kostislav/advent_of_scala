package cz.judas.jan.autumn

import cz.judas.jan.advent.InputData
import org.junit.jupiter.api.Test
import org.scalatest.Assertions.assert

class Problem3Test:
  private val input = InputData.fromString("xcdXCjQKMDAwMDAwMDA1MQowMDAwMDAwMDU0CjAwMDAwMDAxNDQKMDAwMDAwMDE4MQoKMAoKewogICAgImtpbmQiID0+ICJyb29cdCIKICAgICJzaXplIiA9PiAxMTEKICAgICJjb250ZW50IiA9PiBmYWxzZQogICAgImVuY29kaW5nIiA9PiBuaWwKfQoKW3RydWU7IGZhbHNlOyB0cnVlOyB0cnVlOyB0cnVlOyAtMV0KCiJhY3ciCg==")

  @Test
  def exampleWorks(): Unit =
    assert(Problem3.solve(input) == 732L)
