package observatory

import java.io.File

import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.{Image, Pixel}
import org.apache.commons.math3.util.FastMath._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val ImageAlpha = 127
  val ImageScale = 1d
  val ImageWidth = (256 / ImageScale).toInt
  val ImageHeight = (256 / ImageScale).toInt
  val SubtileZoomLevel = log(2.0, ImageWidth).toInt
  val ZoomMax = 3
  val StationsFile = "/stations.csv"

  def main(args: Array[String]): Unit = {

    def generateTilesByYear(year: Year): Unit = {

      val yearFile = s"/$year.csv"

      print("Extraction.locateTemperatures ... ")
      val dateLocTemp = Extraction.locateTemperatures(year, StationsFile, yearFile)
      println("done")
      print("Extraction.locationYearlyAverageRecords ... ")
      val locTemp = Extraction.locationYearlyAverageRecords(dateLocTemp)
      println("done!")
      val yearData = Iterable((year, locTemp))

      generateTiles(yearData, imageGenerator)
    }

    generateTilesByYear(2015)
  }

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location =
    tile.location

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val x0 = pow(2.0, SubtileZoomLevel).toInt * tile.x
    val y0 = pow(2.0, SubtileZoomLevel).toInt * tile.y
    val zoom = tile.zoom + SubtileZoomLevel

    val pixels = for {
      y <- (0 until ImageHeight).par
      x <- (0 until ImageWidth).par
    } yield {
      // Generating subtile
      val xPos = x0 + x
      val yPos = y0 + y
      val subtile = Tile(xPos, yPos, zoom)

      // Generating pixel of subtile
      val location = tileLocation(subtile)
      val temperature = Visualization.predictTemperature(temperatures, location)
      val color = Visualization.interpolateColor(colors, temperature)
      Pixel(color.red, color.green, color.blue, ImageAlpha)
    }

    Image(ImageWidth, ImageHeight, pixels.toArray)
      .scale(ImageScale)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {

    for {
      (year, data) <- yearlyData.par
      zoom <- (0 to ZoomMax).par
      tile <- tilesInZoom(zoom).par
    } {
      println(s"generation zoom ${zoom} on year ${year} and tile (${tile.x},${tile.y}) ...")
      generateImage(year, tile, data)
      println(s"done zoom ${zoom} on year ${year} and tile (${tile.x},${tile.y}) !")
    }
  }

  def tilesInZoom(zoom: Int): Iterable[Tile] = {
    val dim = pow(2, zoom).toInt
    for {
      y <- 0 until dim
      x <- 0 until dim
    } yield Tile(x, y, zoom)
  }

  def imageGenerator(year: Year, t: Tile, data: Iterable[(Location, Temperature)]): Unit = {
    val image = tile(data, ColorScale, t)
    saveImageTile(image, year, t)
  }

  def saveImageTile(image: Image, year: Year, tile: Tile): Unit = {
    val path = tilePath(year, tile)
    val imageFile = new File(path)

    // create files
    val parentFile = imageFile.getParentFile
    if (!parentFile.exists()) parentFile.mkdirs()
    if (!imageFile.exists()) imageFile.createNewFile()

    image.output(imageFile)(PngWriter.NoCompression)
    () // just Unit return
  }

  def tilePath(year: Year, tile: Tile): String =
    s"target/temperatures/${year}/${tile.zoom}/${tile.x}-${tile.y}.png"

}
