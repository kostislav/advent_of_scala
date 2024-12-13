package cz.judas.jan.advent


def solveSystem(
  a1: Long, b1: Long, c1: Long,
  a2: Long, b2: Long, c2: Long,
): Option[(Long, Long)] =
  val x = (c1 * b2 - c2 * b1) / (a1 * b2 - b1 * a2)
  val y = (c1 - a1 * x) / b1
  if a1 * x + b1 * y == c1 && a2 * x + b2 * y == c2 then
    Some((x, y))
  else
    None
