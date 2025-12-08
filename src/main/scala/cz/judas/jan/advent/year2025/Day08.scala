package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, pattern}

import scala.collection.mutable

object Day08:
  def part1(input: InputData): Long =
    part1(input, 1000)

  def part1(input: InputData, numConnections: Int): Long =
    val boxes = input.linesAs[Position3d].toSet
    val edgeIterator = edgesByDistance(boxes).iterator
    val graph = ConnectedComponents[Position3d]()
    boxes.foreach(graph.addNode)

    (0 until numConnections).foreach: _ =>
      val shortestEdge = edgeIterator.next()
      graph.addEdge(shortestEdge)

    graph.components.toSeq.map(_.size).sorted.reverse.take(3).product

  def part2(input: InputData): Long =
    val boxes = input.linesAs[Position3d].toSet
    val edgeIterator = edgesByDistance(boxes).iterator

    val graph = ConnectedComponents[Position3d]()
    boxes.foreach(graph.addNode)

    var bleh = 0L
    while graph.numComponents > 1 do
      val shortestEdge = edgeIterator.next()
      graph.addEdge(shortestEdge)
      bleh = shortestEdge.node1.x * shortestEdge.node2.x

    bleh

  private def edgesByDistance(boxes: Iterable[Position3d]): Seq[UndirectedEdge[Position3d]] =
    boxes.flatMap(box1 => boxes.filter(box2 => box2 != box1).map(box2 => UndirectedEdge(box1, box2)))
      .toSeq
      .sortBy(edge => edge.node1.squareDistance(edge.node2))


class ConnectedComponents[T]:
  private val componentByNode = mutable.HashMap[T, Int]()
  private val nodesByComponent = mutable.HashMap[Int, Set[T]]()

  def addNode(node: T): Unit =
    val id = nodesByComponent.size + 1
    componentByNode.put(node, id)
    nodesByComponent.put(id, Set(node))

  def addEdge(edge: UndirectedEdge[T]): Unit =
    val component1Id = componentByNode(edge.node1)
    val component2Id = componentByNode(edge.node2)
    if component1Id != component2Id then
      nodesByComponent(component2Id).foreach(node => componentByNode.put(node, component1Id))
      nodesByComponent.put(component1Id, nodesByComponent(component1Id) ++ nodesByComponent(component2Id)) // TODO mutable set
      nodesByComponent.remove(component2Id)

  def numComponents: Int =
    nodesByComponent.size

  def components: Iterable[Set[T]] =
    nodesByComponent.values


class UndirectedEdge[T](val node1: T, val node2: T):
  override def equals(o: Any): Boolean =
    val other = o.asInstanceOf[UndirectedEdge[T]]
    (other.node1 == this.node1 && other.node2 == this.node2) || (other.node2 == this.node1 && other.node1 == this.node2)

  override def hashCode(): Int =
    node1.hashCode() * node2.hashCode()


@pattern("{},{},{}")
case class Position3d(x: Long, y: Long, z: Long):
  def squareDistance(other: Position3d): Long =
    val xDiff = x - other.x
    val yDiff = y - other.y
    val zDiff = z - other.z
    xDiff * xDiff + yDiff * yDiff + zDiff * zDiff
