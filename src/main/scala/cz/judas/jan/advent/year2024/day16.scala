package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, AutoMap, InputData, Position, RelativePosition, getOnlyElement, shortestPath}

import scala.collection.mutable

object Day16:
  def part1(input: InputData): Int =
    val maze = Array2d.fromInput(input)
    val start = State(maze.positionOfOnly('S'), RelativePosition.RIGHT)
    val targetPosition = maze.positionOfOnly('E')
    shortestPath(start)(_.position == targetPosition)(neighbors(maze, _)).get

  def part2(input: InputData): Int =
    val maze = Array2d.fromInput(input)
    val start = State(maze.positionOfOnly('S'), RelativePosition.RIGHT)
    val targetPosition = maze.positionOfOnly('E')

    val toVisit = mutable.PriorityQueue[(State, Int)]()(Ordering.by[(State, Int), Int](_._2).reverse)
    val dist = mutable.HashMap[State, Int]()
    val prev = AutoMap[State, mutable.HashSet[State]](_ => mutable.HashSet())
    var result: Option[Int] = None
    toVisit.addOne((start, 0))
    dist.put(start, 0)
    while toVisit.nonEmpty do
      val (closest, distance) = toVisit.dequeue()
      if closest.position == targetPosition then
        result = Some(distance)
      else if dist(closest) == distance then
        neighbors(maze, closest).iterator.foreach: (neighbor, weight) =>
          val neighborWeight = distance + weight
          if result.forall(_ >= neighborWeight) then
            val previousNeighborWeight = dist.get(neighbor)
            if previousNeighborWeight.forall(_ >= neighborWeight) then
              toVisit.addOne((neighbor, neighborWeight))
              val reverseNeighbors = prev.getOrCreate(neighbor)
              if previousNeighborWeight.exists(_ > neighborWeight) then
                reverseNeighbors.clear()
              dist.put(neighbor, neighborWeight)
              reverseNeighbors.addOne(closest)

    RelativePosition.horizontalDirections.map(State(targetPosition, _)).flatMap: x =>
      if dist.contains(x) then
        bleh(maze, x, dist, prev.toMap, Seq.empty[State]).flatten
      else
        Seq.empty
    .toSet.map(_.position).size


  private def bleh(maze: Array2d, node: State, dist: mutable.Map[State, Int], prev: Map[State, mutable.Set[State]], path: Seq[State]): Seq[Seq[State]] =
    if dist(node) == 0 then
      Seq(path :+ node)
    else
      prev(node)
        .toSeq
        .flatMap(parent => bleh(maze, parent, dist, prev, path :+ node))

  private def neighbors(maze: Array2d, current: State): Iterator[(State, Int)] =
    val State(position, direction) = current
    val neighbors = mutable.ArrayBuffer[(State, Int)]()
    if maze.get(position + direction).exists(_ != '#') then
      neighbors += State(position + direction, direction) -> 1
    neighbors += State(position, direction.rotateRight) -> 1000
    neighbors += State(position, direction.rotateLeft) -> 1000
    neighbors.iterator


case class State(position: Position, direction: RelativePosition)
