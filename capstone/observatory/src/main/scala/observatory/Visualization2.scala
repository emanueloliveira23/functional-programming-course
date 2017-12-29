package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import org.apache.commons.math3.util.FastMath._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  val ImageWidth = 256
  val ImageHeight = 256

  def main(args: Array[String]): Unit = {

    val StationsFile = "/stations.csv"

    // Common
    def temperatures(year: Year): Iterable[(Location, Temperature)] = {
      println(s"Rading temperatures of $year...")
      val yearFile = s"/$year.csv"
      val result = Extraction
        .locateTemperatures(year, StationsFile, yearFile)
        .map { case (_, location, temperature) =>
          location -> temperature
        }
      println(s"$year read done!")
      result
    }

    def computeNormals(): GridLocation => Temperature = {
      println("Computing normals...")
      val yearTemperatures = for {
        year <- (1975 to 1989).par
      } yield temperatures(year)
      val result = Manipulation.average( yearTemperatures.seq )
      println("Normals computed!")
      result
    }


    def computeDeviations(year: Year, normals: GridLocation => Temperature): GridLocation => Temperature = {
      println(s"Computing deviations of $year ...")
      val yearTemperatures = temperatures(year)
      val result = Manipulation.deviation(yearTemperatures, normals)
      println(s"$year deviations computed!")
      result
    }

    def run(): Unit = {
      val normals = computeNormals()

      for (year <- (1990 to 2015).par) {

        println(s"Making $year data ...")
        val deviations = computeDeviations(year, normals)
        println(s"$year data made!")

        println(s"Generating tiles of $year...")
        Interaction.generateTiles(
          Seq( year -> deviations ),
          (year: Year, tile: Tile, grid: GridLocation => Temperature) => {
            val image = visualizeGrid(grid, ColorScale2, tile)
            Interaction.saveImageTile(image, year, tile)
          }
        )
        println(s"$year tiles generated!")

      }
    }

    run()
  }

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) +
    d10 * point.x * (1 - point.y) +
    d01 * (1 - point.x) * point.y +
    d11 * point.x * point.y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {

    val pixels = pixelLocations(tile).par.map{
      case (pos,pixelLocation) => {
        val (latMin, latMax) = (floor(pixelLocation.lat).toInt, ceil(pixelLocation.lat).toInt)
        val (lonMin, lonMax) = (floor(pixelLocation.lon).toInt, ceil(pixelLocation.lon).toInt)
        val d00 = grid(GridLocation(latMax, lonMin))
        val d01 = grid(GridLocation(latMin, lonMin))
        val d10 = grid(GridLocation(latMax, lonMax))
        val d11 = grid(GridLocation(latMin, lonMax))
        val xFraction = pixelLocation.lon - lonMin
        val yFraction = latMax - pixelLocation.lat
        val cellPoint = CellPoint(xFraction, yFraction)
        val biInterpolation = bilinearInterpolation(cellPoint, d00, d01, d10, d11)
        val color = Visualization.interpolateColor(colors, biInterpolation)
        val pixel = Pixel(color.red, color.green, color.blue, 255)
        pos -> pixel
      }
    }
    .seq
    .sortBy(_._1)
    .map(_._2)

    Image(ImageWidth, ImageHeight, pixels.toArray)
  }

  def pixelLocations(tile: Tile) = {
    for{
      xPixel <- 0 until ImageWidth
      yPixel <- 0 until ImageHeight
    } yield (
      xPixel + yPixel * ImageWidth,
      Tile(
        (xPixel.toDouble / ImageWidth + tile.x).toInt,
        (yPixel.toDouble / ImageHeight + tile.y).toInt,
        tile.zoom
      ).location
    )
  }

}
