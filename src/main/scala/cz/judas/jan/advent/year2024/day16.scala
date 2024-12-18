package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, getOnlyElement, shortestPath}

import scala.collection.mutable

object Day16:
  def part1(input: InputData): Int =
    val maze = Array2d.fromInput(input)
    val start = State(maze.indices.filter(position => maze(position) == 'S').getOnlyElement, RelativePosition.RIGHT)
    val targetPosition = maze.indices.filter(position => maze(position) == 'E').getOnlyElement  // TODO dedup?
    shortestPath(start)(_.position == targetPosition)(neighbors(maze, _)).get

  def part2(input: InputData): Int =
    val maze = Array2d.fromInput(input)
    val start = State(maze.indices.filter(position => maze(position) == 'S').getOnlyElement, RelativePosition.RIGHT)
    val targetPosition = maze.indices.filter(position => maze(position) == 'E').getOnlyElement // TODO dedup?

    val toVisit = mutable.PriorityQueue[(State, Int)]()(Ordering.by[(State, Int), Int](_._2).reverse)
    val dist = mutable.HashMap[State, ReverseNeighbors]()
    var result: Option[Int] = None
    toVisit.addOne((start, 0))
    dist.put(start, ReverseNeighbors(0))
    while toVisit.nonEmpty do
      val (closest, distance) = toVisit.dequeue()
      if closest.position == targetPosition then
        result = Some(distance)
      else if dist(closest).distance == distance then
        neighbors(maze, closest).iterator.foreach: (neighbor, weight) =>
          val neighborWeight = distance + weight
          if result.forall(_ >= neighborWeight) then
            if !dist.contains(neighbor) then
              dist.put(neighbor, ReverseNeighbors(neighborWeight))
              toVisit.addOne((neighbor, neighborWeight))
            else
              if dist(neighbor).distance > neighborWeight then
                toVisit.addOne((neighbor, neighborWeight))
                dist(neighbor).distance = neighborWeight
            dist(neighbor).parents += closest

    RelativePosition.horizontalDirections.map(State(targetPosition, _)).flatMap: x =>
      if dist.contains(x) then
        bleh(maze, x, dist, Seq.empty[State]).flatten
      else
        Seq.empty
    .toSet.map(_.position).size


  private def bleh(maze: Array2d, node: State, dist: mutable.Map[State, ReverseNeighbors], path: Seq[State]): Seq[Seq[State]] =
    val currentDist = dist(node)
    if currentDist.distance == 0 then
      Seq(path :+ node)
    else
      currentDist.parents
        .toSeq
        .filter(parent => dist(parent).distance + neighbors(maze, parent).toMap.apply(node) == currentDist.distance)
        .flatMap(parent => bleh(maze, parent, dist, path :+ node))

  private def neighbors(maze: Array2d, current: State): Iterator[(State, Int)] =
    val State(position, direction) = current
    val neighbors = mutable.ArrayBuffer[(State, Int)]()
    if maze.get(position + direction).exists(_ != '#') then
      neighbors += State(position + direction, direction) -> 1
    neighbors += State(position, direction.rotateRight) -> 1000
    neighbors += State(position, direction.rotateLeft) -> 1000
    neighbors.iterator


case class State(position: Position, direction: RelativePosition)


class ReverseNeighbors(var distance: Int):
  val parents: mutable.Set[State] = mutable.HashSet()
