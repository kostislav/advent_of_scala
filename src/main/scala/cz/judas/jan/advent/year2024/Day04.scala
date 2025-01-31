package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.*

object Day04:
  def part1(input: InputData): Int =
    val wordSearch = input.asArray2d
    val directions = RelativePosition.allDirections

    wordSearch.indices
      .map: startingPosition =>
        directions.count: direction =>
          matches(wordSearch, startingPosition, direction, "XMAS")
      .sum

  def part2(input: InputData): Int =
    val wordSearch = input.asArray2d
    val diagonalCombinations = Seq(RelativePosition.UP_RIGHT, RelativePosition.DOWN_LEFT)
      .cartesianProduct(Seq(RelativePosition.UP_LEFT, RelativePosition.DOWN_RIGHT))

    wordSearch.indices
      .count: startingPosition =>
        diagonalCombinations.exists:
          case (first, second) =>
            matches(wordSearch, startingPosition - first, first, "MAS")
              && matches(wordSearch, startingPosition - second, second, "MAS")

  private def matches(wordSearch: Array2d, startingPosition: Position, direction: RelativePosition, word: String): Boolean =
    word.indices.forall(i => wordSearch.get(startingPosition + direction * i).contains(word(i)))
