package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import org.apache.commons.math3.util.FastMath._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val EarthRadius = 6371.8
  val DistancePower = 6.0

  // Image propose to this class
  val LongitudinalDim = 180
  val LatitudinalDim = 90
  val ImageWidth = 360
  val ImageHeight = 180
  val ImageCenterX = floor(ImageWidth / 2).toInt
  val ImageCenterY = floor(ImageHeight / 2).toInt

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    // from: https://en.wikipedia.org/wiki/Inverse_distance_weighting#Basic_form
    val result = temperatures

      // init phase:
      // turn in a parallel collection
      .toSeq.par

      // map phase:
      // map each example to distance from location argument and temperature
      .map { case (loc, temperature) =>
        ( distance(loc, location), temperature )
      }

      // reduce phase:
      // start with the accumulator tuple (numerator, divisor, found zero distance flag)
      // for each example do:
      // if already found a zero distance, return the temperature tuple: (temperature of zero distance, 0, true)
      // if current distance is zero, return (current temperature, 0, true)
      // else return "happy day" tuple: (numerator plus temp. times dist., divider plus distance, false)
      .foldLeft((0.0, 0.0, false)) { case (acc, tuple) =>
        val ( numerator, divider, foundZeroD ) = acc
        if (foundZeroD) acc
        else {
          val ( distance, temperature ) = tuple
          if (isZero(distance)) {
            (temperature, 0.0, true)
          } else {
            val distanceWeight = weight(distance)
            ( numerator +  distanceWeight * temperature,
              divider + distanceWeight,
              false )
          }
        }
      }

    // finally, check result
    // if "happy day" tuple, return the division
    // if temperature tuple: return it.
    result match {
      case (numerator, divider, false) => numerator / divider
      case (temperature, _, true) => temperature
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    // find the points to interpolate (one or two)
    val sortedPoints = points.toList.sortBy(_._1)

    // extreme checks
    if ( value <= sortedPoints.head._1 ) sortedPoints.head._2
    else if (value >= sortedPoints.last._1) sortedPoints.last._2
    else {
      // interpolate two points
      val (lower, higher) = sortedPoints.partition(_._1 < value)
      val (aTemp, aColor) = lower.last
      val (bTemp, bColor) = higher.head
      val ratio = (value - aTemp) / (bTemp - aTemp)
      Color(
        round(aColor.red + (bColor.red - aColor.red) * ratio).toInt,
        round(aColor.green + (bColor.green - aColor.green) * ratio).toInt,
        round(aColor.blue + (bColor.blue - aColor.blue) * ratio).toInt
      )
    }

  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val pixels = for {
      y <- 0 until ImageHeight
      x <- 0 until ImageWidth
    } yield {
      val location = getLocation(x, y)
      val temperature = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, temperature)
      Pixel(color.red, color.green, color.blue, 255)
    }

    Image(ImageWidth, ImageHeight, pixels.toArray)
  }

  // Util

  /**
    * Check if two points are antipodes.
    * Source: https://en.wikipedia.org/wiki/Antipodes#Mathematical_description.
    * @param a One of points.
    * @param b Another point.
    * @return true if a and b are antipodes, false otherwise.
    */
  def isAntipodes(a: Location, b: Location): Boolean =
    a == Location(-b.lat, (b.lon + LongitudinalDim) % LongitudinalDim) ||
    a == Location(-b.lat, (b.lon - LongitudinalDim) % LongitudinalDim)

  /**
    * Computes the Grand Circle Distance, between two points.
    * Extracted from: https://en.wikipedia.org/wiki/Great-circle_distance
    * @param a One of points.
    * @param b Another point.
    * @return Grand Circle Distance between a and b.
    */
  def distance(a: Location, b: Location): Double = {

    val delta = {
      if (a == b) {
        0
      } else if (isAntipodes(a, b)) {
        PI
      } else {
        val aLat = toRadians(a.lat)
        val aLon = toRadians(a.lon)
        val bLat = toRadians(b.lat)
        val bLon = toRadians(b.lon)
        val aLatSin = sin(aLat)
        val bLatSin = sin(bLat)
        val aLatCos = cos(aLat)
        val bLatCos = cos(bLat)
        val abLonCos = cos( abs( aLon - bLon ) )
        acos(aLatSin * bLatSin + aLatCos * bLatCos * abLonCos)
      }
    }

    EarthRadius * delta
  }

  def weight(distance: Double): Double =
    1d / pow(distance, DistancePower)

  def isZero(distance: Double): Boolean =
    distance < 1d

  def getLocation(x: Int, y: Int): Location = {
    val lat = ImageCenterY - y
    val lon = x - ImageCenterX
    Location(lat, lon)
  }
}

