package cz.judas.jan.advent.year2025

import cz.judas.jan.advent.{InputData, pattern, selfProduct}

import scala.collection.mutable

object Day08:
  def part1(input: InputData): Long =
    part1(input, 1000)

  def part1(input: InputData, numConnections: Int): Long =
    val (graph, sortedEdges) = parseInput(input)
    val edgeIterator = sortedEdges.iterator

    (0 until numConnections).foreach: _ =>
      val shortestEdge = edgeIterator.next()
      graph.addEdge(shortestEdge)

    graph.components.toSeq.map(_.size).sorted.reverse.take(3).product

  def part2(input: InputData): Long =
    val (graph, sortedEdges) = parseInput(input)
    val edgeIterator = sortedEdges.iterator

    var bleh = 0L
    while graph.numComponents > 1 do
      val shortestEdge = edgeIterator.next()
      graph.addEdge(shortestEdge)
      bleh = shortestEdge.node1.x * shortestEdge.node2.x

    bleh

  private def parseInput(input: InputData): (ConnectedComponents[Position3d], Seq[UndirectedEdge[Position3d]]) =
    val boxes = input.linesAs[Position3d].toSeq
    val graph = ConnectedComponents[Position3d]()
    boxes.foreach(graph.addNode)
    (graph, edgesByDistance(boxes))

  private def edgesByDistance(boxes: Seq[Position3d]): Seq[UndirectedEdge[Position3d]] =
    boxes.selfProduct(onlyDifferent = true)
      .map(UndirectedEdge(_, _))
      .sortBy(edge => edge.node1.squareDistance(edge.node2))


class ConnectedComponents[T]:
  private val componentByNode = mutable.Map[T, Int]()
  private val nodesByComponent = mutable.Map[Int, mutable.Set[T]]()

  def addNode(node: T): Unit =
    val id = nodesByComponent.size + 1
    componentByNode.put(node, id)
    nodesByComponent.put(id, mutable.Set(node))

  def addEdge(edge: UndirectedEdge[T]): Unit =
    val component1Id = componentByNode(edge.node1)
    val component2Id = componentByNode(edge.node2)
    if component1Id != component2Id then
      nodesByComponent(component2Id).foreach(node => componentByNode.put(node, component1Id))
      nodesByComponent(component1Id).addAll(nodesByComponent(component2Id))
      nodesByComponent.remove(component2Id)

  def numComponents: Int =
    nodesByComponent.size

  def components: Iterable[Set[T]] =
    nodesByComponent.values.map(_.toSet)


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
