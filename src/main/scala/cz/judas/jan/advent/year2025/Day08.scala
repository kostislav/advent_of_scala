package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, pattern}

import scala.collection.mutable

object Day08:
  def part1(input: InputData): Long =
    part1(input, 1000)

  def part1(input: InputData, numConnections: Int): Long =
    val boxes = input.linesAs[Position3d].toSet
    var edges = boxes.flatMap(box1 => boxes.filter(box2 => box2 != box1).map(box2 => Set(box1, box2)))
    val componentByBox = mutable.HashMap[Position3d, Int]()
    val boxesByComponents = mutable.HashMap[Int, Set[Position3d]]()
    boxes.zipWithIndex.foreach: (box, id) =>
      componentByBox.put(box, id)
      boxesByComponents.put(id, Set(box))

    val processed = mutable.HashSet[Set[Position3d]]()

    (0 until numConnections).foreach: _ =>
      val closestPair = findCloseestPair(edges).toList
      val box1 = closestPair(0)
      val box2 = closestPair(1)
      edges = edges - Set(box1, box2)
      val component1Id = componentByBox(box1)
      val component2Id = componentByBox(box2)
      if component1Id != component2Id then
        boxesByComponents(component2Id).foreach(box => componentByBox.put(box, component1Id))
        boxesByComponents.put(component1Id, boxesByComponents(component1Id) ++ boxesByComponents(component2Id))
        boxesByComponents.remove(component2Id)

    boxesByComponents.values.toSeq.map(_.size).sorted.reverse.take(3).product

  def part2(input: InputData): Long =
    val boxes = input.linesAs[Position3d].toSet
    var edges = boxes.flatMap(box1 => boxes.filter(box2 => box2 != box1).map(box2 => Set(box1, box2)))
    val componentByBox = mutable.HashMap[Position3d, Int]()
    val boxesByComponents = mutable.HashMap[Int, Set[Position3d]]()
    boxes.zipWithIndex.foreach: (box, id) =>
      componentByBox.put(box, id)
      boxesByComponents.put(id, Set(box))

    val processed = mutable.HashSet[Set[Position3d]]()

    var bleh = 0L
    while boxesByComponents.size > 1 do
      val closestPair = findCloseestPair(edges).toList
      val box1 = closestPair(0)
      val box2 = closestPair(1)
      edges = edges - Set(box1, box2)
      val component1Id = componentByBox(box1)
      val component2Id = componentByBox(box2)
      if component1Id != component2Id then
        boxesByComponents(component2Id).foreach(box => componentByBox.put(box, component1Id))
        boxesByComponents.put(component1Id, boxesByComponents(component1Id) ++ boxesByComponents(component2Id))
        boxesByComponents.remove(component2Id)
        bleh = box1.x * box2.x

    bleh

  private def findCloseestPair(edges: Set[Set[Position3d]]): Set[Position3d] =
    edges.minBy: edge =>
      val bleh = edge.toList
      bleh(0).squareDistance(bleh(1))

@pattern("{},{},{}")
case class Position3d(x: Long, y: Long, z: Long):
  def squareDistance(other: Position3d): Long =
    val xDiff = x - other.x
    val yDiff = y - other.y
    val zDiff = z - other.z
    xDiff * xDiff + yDiff * yDiff + zDiff * zDiff
