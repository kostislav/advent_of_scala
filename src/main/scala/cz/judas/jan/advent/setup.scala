package cz.judas.jan.advent


//TODO dedup
@main
def setup(year: Int, day: Int): Unit =
  val template = Path("template").readString()
    .replace("year2024", s"year${year}")
    .replace("Day01", f"Day${day}%02d")
  val srcDir = Path(s"src/main/scala/cz/judas/jan/advent/year${year}")
  srcDir.createDirectories()
  val srcFile = srcDir / f"day${day}%02d.scala"
  srcFile.writeString(template)

  val testTemplate = Path("test_template").readString()
    .replace("year2024", s"year${year}")
    .replace("Day01", f"Day${day}%02d")
  val testDir = Path(s"src/test/scala/cz/judas/jan/advent/year${year}")
  testDir.createDirectories()
  val testFile = testDir / f"Day${day}%02dTest.scala"
  testFile.writeString(testTemplate)

  val mainFile = Path("src/main/scala/cz/judas/jan/advent/main.scala")
  val mainFileContent = mainFile.readString().linesIterator
    .map: line =>
      if line.contains("val result = ") then
        s"  val result = run(year = ${year}, day = ${day}, part = 1)"
      else
        line
    .mkString("\n")
  mainFile.writeString(mainFileContent)

  ProcessBuilder("git", "add", srcFile.toString, testFile.toString).start().waitFor()


