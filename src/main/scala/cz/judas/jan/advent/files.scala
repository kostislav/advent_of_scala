package cz.judas.jan.advent

import java.nio.file.{Files, Path, Paths}
import scala.annotation.targetName


def path(path: String): Path =
  Paths.get(path)


extension (path: Path)
  def readString(): String =
    Files.readString(path)

  def writeString(content: String): Unit =
    Files.writeString(path, content)

  def createDirectories(): Unit =
    Files.createDirectories(path)
  
  def exists: Boolean =
    path.toFile.exists()

  @targetName("child")
  def /(child: String): Path =
    path.resolve(child)
