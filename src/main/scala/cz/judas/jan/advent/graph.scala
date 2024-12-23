package cz.judas.jan.advent

import scala.collection.mutable


def linearizeDag[T](graph: Map[T, Iterable[T]]): Seq[T] =
  val remaining = graph
    .view.mapValues(neighbors => mutable.HashSet.newBuilder[T].addAll(neighbors).result())
    .toHashMap
  val result = mutable.ArrayBuffer[T]()

  while remaining.nonEmpty do
    val next = remaining.filter((key, values) => values.isEmpty).iterator.next()._1
    result += next
    remaining.remove(next)
    remaining.values.foreach(neighbors => neighbors.remove(next))

  result.toSeq


def linearizeDag[T](first: T, graph: Map[T, Iterable[T]]): Seq[T] =
  linearizeDag(graph ++ Map(first -> Seq.empty))


def shortestPath[T](start: T)(isTarget: T => Boolean)(edges: T => IterableOnce[(T, Int)]): Option[Int] =
  val toVisit = mutable.PriorityQueue[(T, Int)]()(Ordering.by[(T, Int), Int](_._2).reverse)
  val dist = mutable.HashMap[T, Int]()
  var result: Option[Int] = None
  toVisit.addOne((start, 0))
  dist.put(start, 0)
  while toVisit.nonEmpty && result.isEmpty do
    val (closest, distance) = toVisit.dequeue()
    if isTarget(closest) then
      result = Some(distance)
    else if dist(closest) == distance then
      edges(closest).iterator.foreach: (neighbor, weight) =>
        val neighborWeight = distance + weight
        if dist.get(neighbor).forall(neighborDistance => neighborDistance > neighborWeight) then
          toVisit.addOne((neighbor, neighborWeight))
          dist.put(neighbor, neighborWeight)

  result


def shortestPath[T](start: T, target: T)(edges: T => IterableOnce[(T, Int)]): Option[Int] =
  shortestPath(start)(_ == target)(edges)


def maximumClique[T](graph: UndirectedGraph[T]): Set[T] =
  def bleh(r: Set[T], p: mutable.Set[T], x: mutable.Set[T]): Option[Set[T]] =
    if p.isEmpty && x.isEmpty then
      Some(r)
    else
      p
        .iterator
        .flatMap: v =>
          val neighbors = graph.neighborsOf(v)
          val done = bleh(r + v, mutable.HashSet.from(p).intersect(neighbors), mutable.HashSet.from(p).intersect(neighbors))
          p -= v
          x -= v
          done
        .maxByOption(_.size)

  bleh(Set.empty, mutable.HashSet.from(graph.nodes), mutable.HashSet()).getOrElse(Set.empty)


class UndirectedGraph[T](neighbors: Map[T, Set[T]]):
  def nodes: Set[T] =
    neighbors.keys.toSet

  def neighborsOf(node: T): Set[T] =
    neighbors(node)

object UndirectedGraph:
  def builder[T]: Builder[T] =
    Builder()

  class Builder[T]:
    private val neighbors = AutoMap[T, mutable.Set[T]](_ => mutable.HashSet())

    def addEdge(node1: T, node2: T): Unit =
      neighbors.getOrCreate(node1) += node2
      neighbors.getOrCreate(node2) += node1

    def build: UndirectedGraph[T] =
      UndirectedGraph(
        neighbors.toMap.transformValues(_.toSet)
      )
