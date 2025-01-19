package cz.judas.jan.advent

import java.nio.file.Path


@main
def setup(year: Int, day: Int): Unit =
  val packagePath = s"cz/judas/jan/advent/year${year}"

  createFromTemplate(path("template"), path("src/main/scala") / packagePath / f"Day${day}%02d.scala", year, day)
  createFromTemplate(path("test_template"), path("src/test/scala") / packagePath / f"Day${day}%02dTest.scala", year, day)

  transformLines(path("src/main/scala/cz/judas/jan/advent/main.scala")): line =>
    if line.contains("val (result, time) = ") then
      s"  val (result, time) = run(year = ${year}, day = ${day}, part = 1)"
    else
      line


private def createFromTemplate(template: Path, targetFile: Path, year: Int, day: Int): Unit =
  val templateString = template.readString()
    .replace("year2024", s"year${year}")
    .replace("Day01", f"Day${day}%02d")
  targetFile.getParent.createDirectories()
  targetFile.writeString(templateString)

  ProcessBuilder("git", "add", targetFile.toString).start().waitFor()


private def transformLines(file: Path)(transformation: String => String): Unit =
  val contents = file.readString().linesIterator
    .map(transformation)
    .mkString("\n")
  file.writeString(contents)
