package cz.judas.jan.autumn

import cz.judas.jan.advent.{Array2d, InputData, Position, RelativePosition, shortestPath}

object Problem4:
  def solve(input: InputData): Int =
    val maze = input.asArray2d
    val start = State(
      guy = maze.positionOfOnly('X'),
      box = maze.positionOfOnly('*'),
    )
    val end = maze.positionOfOnly('T')

    shortestPath(start)(_.box == end)(next(_, maze)).get

  private def next(current: State, maze: Array2d): Seq[(State, Int)] =
    RelativePosition.horizontalDirections.flatMap: direction =>
      val nextGuy = current.guy + direction
      if isFreeTile(maze.get(nextGuy)) then
        if nextGuy == current.box then
          if isFreeTile(maze.get(nextGuy + direction)) then
            Some(State(nextGuy, current.box + direction), 1)
          else
            None
        else
          Some(State(nextGuy, current.box), 1)
      else
        None

  private def isFreeTile(c: Option[Char]): Boolean =
    c.exists(cc => cc != '#')


case class State(
  guy: Position,
  box: Position,
)