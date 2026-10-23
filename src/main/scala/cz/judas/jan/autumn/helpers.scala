package cz.judas.jan.autumn

import cz.judas.jan.advent.InputData

import java.nio.file.{Files, Paths}

def autumnInputData(day: Int): InputData =
  InputData.fromString(Files.readString(Paths.get(s"input/autumn/${day}")))