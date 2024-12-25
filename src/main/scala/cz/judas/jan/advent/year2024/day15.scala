package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{InputData, Position, RelativePosition}

import scala.collection.mutable

object Day15:
  def part1(input: InputData): Int =
    val Input(walls, boxes, robotPosition, moves) = parse(input)
    val boxTile = BoxTile(BoxShape(1), RelativePosition.HERE)
    solve(
      walls,
      boxes.map(position => position -> boxTile).toMap,
      robotPosition,
      moves
    )

  def part2(input: InputData): Int =
    val Input(originalWalls, originalBoxes, originalRobotPosition, moves) = parse(input)
    val wideBox = BoxShape(2)
    solve(
      originalWalls.flatMap(wall => Seq(wall.withColumn(_ * 2), wall.withColumn(_ * 2 + 1))),
      originalBoxes
        .flatMap: position =>
          Seq(
            position.withColumn(_ * 2) -> BoxTile(wideBox, RelativePosition.HERE),
            position.withColumn(_ * 2 + 1) -> BoxTile(wideBox, RelativePosition.LEFT),
          )
        .toMap,
      originalRobotPosition.withColumn(_ * 2),
      moves
    )

  private def parse(input: InputData): Input =
    val Array(warehouseStr, movesStr) = input.whole.split("\n\n")
    val warehouse = InputData.fromString(warehouseStr).asArray2d

    Input(
      warehouse.positionsOf('#'),
      warehouse.positionsOf('O'),
      warehouse.positionOfOnly('@'),
      movesStr.replace("\n", "").map:
        case '^' => RelativePosition.UP
        case 'v' => RelativePosition.DOWN
        case '<' => RelativePosition.LEFT
        case '>' => RelativePosition.RIGHT
    )

  private def solve(walls: Set[Position], initialBoxes: Map[Position, BoxTile], initialRobot: Position, moves: Seq[RelativePosition]): Int =
    var robotPosition = initialRobot
    val boxes = mutable.HashMap.from(initialBoxes)
    moves.foreach: move =>
      val toBeMoved = mutable.HashSet[Position]()
      val backlog = mutable.Queue(robotPosition + move)
      var possible = true
      while possible && backlog.nonEmpty do
        val next = backlog.dequeue()
        if walls.contains(next) then
          possible = false
        else
          val boxTiles = boxes.get(next).map(_.tiles(next)).getOrElse(Set.empty)
          val newlyOccupied = boxTiles.map(_ + move) -- boxTiles
          toBeMoved ++= boxTiles
          backlog.addAll(newlyOccupied)

      if possible then
        robotPosition = robotPosition + move
        toBeMoved.toSeq
          .sortBy(position => position.row * move.rowOffset + position.column * move.columnOffset)(Ordering[Int].reverse)
          .foreach: position =>
            boxes.put(position + move, boxes.remove(position).get)

    boxes
      .flatMap: (position, tile) =>
        if tile.origin == RelativePosition.HERE then
          Some(100 * position.row + position.column)
        else
          None
      .sum

  case class Input(walls: Set[Position], boxes: Set[Position], robot: Position, moves: Seq[RelativePosition])

case class BoxShape(width: Int):
  def tiles(origin: Position): Set[Position] =
    (0 until width)
      .map(i => origin.copy(column = origin.column + i))
      .toSet


case class BoxTile(shape: BoxShape, origin: RelativePosition):
  def tiles(position: Position): Set[Position] =
    shape.tiles(position + origin)
