package cz.judas.jan.advent

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Paths}
import java.time.temporal.ChronoField
import java.time.{Instant, LocalDateTime, ZoneOffset}


class Downloader:
  private val httpClient = HttpClient.newBuilder().build()

  def download(year: Int, day: Int): Unit =
    val session = Files.readString(Paths.get(".session")).trim
    val request = HttpRequest.newBuilder(URI.create(s"https://adventofcode.com/${year}/day/${day}/input"))
      .GET()
      .header("Cookie", s"session=${session}")
      .header("User-Agent", "https://github.com/kostislav/advent_of_scala by snugar.i@gmail.com")
      .build()

    val targetFile = InputData.inputLocation(year, day)
    println(s"Downloading to ${targetFile}")
    httpClient.send(request, HttpResponse.BodyHandlers.ofFile(targetFile))
    println("Done")


@main def downloadCurrent(): Unit =
  val zoneOffset = ZoneOffset.ofHours(1)
  val now = Instant.now.atZone(zoneOffset)
  val year = now.get(ChronoField.YEAR)
  val day = now.get(ChronoField.DAY_OF_MONTH)
  val downloader = Downloader()

  val inputBirthTime = LocalDateTime.of(year, 12, day, 6, 0, 1).atOffset(zoneOffset)
  val delay = inputBirthTime.toInstant.toEpochMilli - now.toInstant.toEpochMilli

  if delay > 0 then
    println(s"Will wait for ${delay}")
    Thread.sleep(delay)
  else
    println("Downloading right away")

  downloader.download(year, day)
