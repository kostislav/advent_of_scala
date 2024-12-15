package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, getOnlyElement}

import scala.collection.mutable

object Day15:
  def part1(input: InputData): Int =
    val Array(warehouseStr, movesStr) = input.whole.split("\n\n")
    val originalWarehouse = Array2d.fromInput(InputData.fromString(warehouseStr))
    val moves = movesStr.replace("\n", "").map:
      case '^' => RelativePosition.UP
      case 'v' => RelativePosition.DOWN
      case '<' => RelativePosition.LEFT
      case '>' => RelativePosition.RIGHT

    val warehouse = MutableArray2d.from(originalWarehouse)
    var robotPosition = originalWarehouse.indices.filter(pos => originalWarehouse(pos) == '@').getOnlyElement

    moves.foreach: move =>
      var candidate = robotPosition + move
      warehouse(candidate) match
        case 'O' =>
          while warehouse(candidate) == 'O' do
              candidate += move
          if warehouse(candidate) == '.' then
            warehouse(candidate) = 'O'
            warehouse(robotPosition) = '.'
            robotPosition += move
            warehouse(robotPosition) = '@'
        case '.' =>
          warehouse(robotPosition) = '.'
          robotPosition += move
          warehouse(robotPosition) = '@'
        case _ =>

    warehouse.indices
      .flatMap: position =>
        if warehouse(position) == 'O' then
          Some(100 * position.row + position.column)
        else
          None
      .sum

  def part2(input: InputData): Int =
    val Array(warehouseStr, movesStr) = input.whole.split("\n\n")

    val originalWarehouse = Array2d.fromInput(InputData.fromString(warehouseStr))
    val moves = movesStr.replace("\n", "").map:
      case '^' => RelativePosition.UP
      case 'v' => RelativePosition.DOWN
      case '<' => RelativePosition.LEFT
      case '>' => RelativePosition.RIGHT

    var robotPosition = Position(0, 0)
    val warehouse = MutableArray2d.empty(originalWarehouse.numRows, originalWarehouse.numColumns * 2, '.')

    originalWarehouse.entries.foreach: (position, value) =>
      value match
        case '#' =>
          warehouse(position.copy(column = position.column * 2)) = '#'
          warehouse(position.copy(column = position.column * 2 + 1)) = '#'
        case '@' =>
          warehouse(position.copy(column = position.column * 2)) = '@'
          robotPosition = position.copy(column = position.column * 2)
        case 'O' =>
          warehouse(position.copy(column = position.column * 2)) = '['
          warehouse(position.copy(column = position.column * 2 + 1)) = ']'
        case _ =>

    moves.foreach: move =>
      warehouse(robotPosition + move) match
//        TODO unify
        case '.' =>
          warehouse(robotPosition) = '.'
          robotPosition += move
          warehouse(robotPosition) = '@'
        case '[' | ']' =>
          if push(robotPosition, move, warehouse) then
            robotPosition += move
            warehouse(robotPosition) = '@'
        case _ =>

    warehouse.indices
      .flatMap: position =>
        if warehouse(position) == '[' then
          Some(100 * position.row + position.column)
        else
          None
      .sum

  private def push(robotPosition: Position, direction: RelativePosition, warehouse: MutableArray2d): Boolean =
    if direction == RelativePosition.LEFT || direction == RelativePosition.RIGHT then
      var candidate = robotPosition + direction
      while warehouse(candidate) == '[' || warehouse(candidate) == ']' do
        candidate += direction
      if warehouse(candidate) == '.' then
        while candidate != robotPosition do
          warehouse(candidate) = warehouse(candidate - direction)
          candidate -= direction
        warehouse(robotPosition) = '.'
        true
      else
        false
    else
      if push(Set(robotPosition), direction, warehouse) then
        warehouse(robotPosition) = '.'
        true
      else
        false

  private def push(boxes: Set[Position], direction: RelativePosition, warehouse: MutableArray2d): Boolean =
    if boxes.exists(position => warehouse(position + direction) == '#') then
      false
    else
      val newBoxes = boxes.flatMap: position =>
        warehouse(position + direction) match
          case '[' => Seq(position + direction, position + direction + RelativePosition.RIGHT)
          case ']' => Seq(position + direction, position + direction + RelativePosition.LEFT)
          case _ => Seq.empty[Position]

      if newBoxes.isEmpty || push(newBoxes, direction, warehouse) then
        boxes.foreach: position =>
          warehouse(position + direction) = warehouse(position)
          warehouse(position) = '.'
        true
      else
        false


class MutableArray2d(values: mutable.ArrayBuffer[Char], val numRows: Int, val numColumns: Int):
  def apply(position: Position): Char =
    values(position.row * numColumns + position.column)

  def update(position: Position, value: Char): Unit =
    values(position.row * numColumns + position.column) = value

  //  TODO dedup
  def indices: Iterator[Position] =
    (0 until numRows).iterator.flatMap(row => (0 until numColumns).map(column => Position(row, column)))


object MutableArray2d:
  def from(immutable: Array2d): MutableArray2d =
    val numRows = immutable.numRows
    val numColumns = immutable.numColumns
    val values = mutable.ArrayBuffer.tabulate(numRows * numColumns)(index => immutable(Position(index / numColumns, index % numColumns)))
    MutableArray2d(values, numRows, numColumns)

  def empty(numRows: Int, numColumns: Int, fillValue: Char): MutableArray2d =
    MutableArray2d(mutable.ArrayBuffer.fill(numRows * numColumns)(fillValue), numRows, numColumns)
