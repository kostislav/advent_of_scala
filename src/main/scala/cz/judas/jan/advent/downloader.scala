package cz.judas.jan.advent

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Paths}
import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.time.temporal.ChronoField
import java.util.concurrent.{Executors, TimeUnit}

@main def downloadCurrent(): Unit =
  val zoneOffset = ZoneOffset.ofHours(1)
  val now = Instant.now.atZone(zoneOffset)
  val year = now.get(ChronoField.YEAR)
  val dayOfMonth = now.get(ChronoField.DAY_OF_MONTH)
  val targetFile = Paths.get(f"input/year${year}/day${dayOfMonth}%02d")
  val session = Files.readString(Paths.get(".session")).trim
  val httpClient = HttpClient.newBuilder().build()
  val request = HttpRequest.newBuilder(URI.create(s"https://adventofcode.com/${year}/day/${dayOfMonth}/input"))
    .GET()
    .header("Cookie", s"session=${session}")
    .header("User-Agent", "https://github.com/kostislav/advent_of_kotlin by snugar.i@gmail.com")
    .build()

  val inputBirthTime = LocalDateTime.of(year, 12, dayOfMonth, 6, 0, 1).atOffset(zoneOffset)
  val delay = inputBirthTime.toInstant.toEpochMilli - now.toInstant.toEpochMilli

  def download(): Unit =
    println(s"Downloading to ${targetFile}")
    httpClient.send(request, HttpResponse.BodyHandlers.ofFile(targetFile))
    println("Done")

  if delay > 0 then
    println(s"Will wait for ${delay}")
    val pool = Executors.newScheduledThreadPool(1)
    val future = pool.schedule(
      new Runnable {
        override def run(): Unit = download()
      },
      delay,
      TimeUnit.MILLISECONDS,
    )
    future.get()
    pool.shutdown()
  else
    println("Downloading right away")
    download()
