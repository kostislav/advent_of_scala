package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, AutoMap, DirectionalPosition, InputData, RelativePosition, shortestPath}

import scala.collection.mutable

object Day16:
  def part1(input: InputData): Int =
    val maze = input.asArray2d.ignoring('#')
    val start = DirectionalPosition(maze.positionOfOnly('S'), RelativePosition.RIGHT)
    val targetPosition = maze.positionOfOnly('E')
    shortestPath(start)(_.position == targetPosition)(neighbors(maze, _)).get

  def part2(input: InputData): Int =
    val maze = input.asArray2d.ignoring('#')
    val start = DirectionalPosition(maze.positionOfOnly('S'), RelativePosition.RIGHT)
    val targetPosition = maze.positionOfOnly('E')

    val toVisit = mutable.PriorityQueue[(DirectionalPosition, Int)]()(Ordering.by[(DirectionalPosition, Int), Int](_._2).reverse)
    val dist = mutable.HashMap[DirectionalPosition, Int]()
    val prev = AutoMap[DirectionalPosition, mutable.HashSet[DirectionalPosition]](_ => mutable.HashSet())
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

    RelativePosition.horizontalDirections.map(DirectionalPosition(targetPosition, _)).flatMap: x =>
      if dist.contains(x) then
        bleh(maze, x, dist, prev.toMap, Seq.empty[DirectionalPosition]).flatten
      else
        Seq.empty
    .toSet.map(_.position).size


  private def bleh(maze: Array2d, node: DirectionalPosition, dist: mutable.Map[DirectionalPosition, Int], prev: Map[DirectionalPosition, mutable.Set[DirectionalPosition]], path: Seq[DirectionalPosition]): Seq[Seq[DirectionalPosition]] =
    if dist(node) == 0 then
      Seq(path :+ node)
    else
      prev(node)
        .toSeq
        .flatMap(parent => bleh(maze, parent, dist, prev, path :+ node))

  private def neighbors(maze: Array2d, current: DirectionalPosition): Iterator[(DirectionalPosition, Int)] =
    val neighbors = mutable.ArrayBuffer[(DirectionalPosition, Int)]()
    val forward = current.walk(1)
    if maze.contains(forward.position) then
      neighbors += forward -> 1
    neighbors += current.turnRight -> 1000
    neighbors += current.turnLeft -> 1000
    neighbors.iterator
