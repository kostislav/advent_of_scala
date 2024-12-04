package cz.judas.jan.advent.year2024

import cz.judas.jan.advent.*

object Day04:
  def part1(input: InputData): Int =
    val wordSearch = WordSearch.fromInput(input)
    RelativePosition.allDirections
      .map: direction =>
        wordSearch.indices
          .count: startingPosition =>
            wordSearch.wordAt(startingPosition, direction, 4) == "XMAS"
      .sum

  def part2(input: InputData): Int =
    val wordSearch = WordSearch.fromInput(input)
    val diagonalCombinations = RelativePosition.diagonalDirections.cartesianProduct(onlyDifferent = true)

    wordSearch.indices
      .count: startingPosition =>
        diagonalCombinations.exists { case (first, second) =>
          wordSearch.wordAt(startingPosition - first, first, 3) == "MAS"
            && wordSearch.wordAt(startingPosition - second, second, 3) == "MAS"
        }


class WordSearch(letters: Array2d):
  def indices: Iterator[Position] =
    letters.indices

  def wordAt(position: Position, direction: RelativePosition, length: Int): String =
    (0 until length).flatMap(i => letters.get(position + direction * i)).mkString("")

object WordSearch:
  def fromInput(input: InputData): WordSearch =
    WordSearch(Array2d.fromInput(input))
