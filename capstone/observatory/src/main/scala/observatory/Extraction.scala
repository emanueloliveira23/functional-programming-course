package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  val FileReaderChunkSize = 4096

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stations = readCSVLines(stationsFile).par
      .filter(validStationLine)
      .map(toStationTuple)

    val temperatures = readCSVLines(temperaturesFile).par
      .map(toTemperatureTuple(year))

    val temperaturesGroups = temperatures.groupBy(_._1)

    stations.flatMap { case (stationId, location) =>
      temperaturesGroups
        .getOrElse(stationId, Iterable.empty)
        .map { case(_, date, temperature) =>
          (date, location, temperature)
        }
    }.seq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records
      .par
      .map(entry => (entry._2, entry._3))
      .groupBy(_._1)
      .map { case (location, entries) =>
        val avg = entries.map(_._2).sum / entries.size
        (location, avg)
      }.seq
  }

  // Utils

  type ID = (String, String)
  type StationTuple = (ID, Location)
  type TemperatureTuple = (ID, LocalDate, Temperature)

  // Columns
  val Id1 = 0
  val Id2 = 1
  val Month = 2
  val Day = 3
  val Temp = 4
  val Lat = 2
  val Lng = 3

  def readCSVLines(source: String) =
    Source.fromInputStream(
      getClass.getResourceAsStream(source)
    )
    .getLines()
    .map(readCsvLine)
    .toSeq

  def readCsvLine(line: String) =
    line.split(",", -1).map(_.trim)

  def validStationLine(cols: Array[String]) =
    cols(Lat).nonEmpty &&
    cols(Lng).nonEmpty

  def toStationTuple(cols: Array[String]): StationTuple =
    (
      key(cols),
      Location(
        cols(Lat).toDouble,
        cols(Lng).toDouble
      )
    )

  def toTemperatureTuple(year: Year)(cols: Array[String]): TemperatureTuple =
    (
      key(cols),
      LocalDate.of(
        year,
        cols(Month).toInt,
        cols(Day).toInt
      ),
      fahrenheitToCelsius(
        cols(Temp).toDouble
      )
    )

  def key(cols: Array[String]) =
    (cols.head /* Id1 */, cols(Id2))

  def fahrenheitToCelsius(fTemp: Double) =
    (fTemp - 32.0) * 5.0/9.0

}
