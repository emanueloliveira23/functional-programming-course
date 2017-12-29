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

    val predictions: Iterable[(Double, Double)] = distanceTemperatureCombi(temperatures, location)

    predictions.find(t => isZero(t._1)) match {
      case Some((_, temp)) => temp
      case _ => inverseDistanceWeighted(predictions, power = 3)
    }
  }

  def distanceTemperatureCombi(temperatures: Iterable[(Location, Double)], location: Location): Iterable[(Double, Double)] = {
    temperatures.map {
      case (otherLocation, temperature) => (distance(location, otherLocation), temperature)
    }
  }

  /**
    * https://en.wikipedia.org/wiki/Inverse_distance_weighting
    *
    * @param distanceTemperatureCombinations
    * @param power
    * @return
    */
  def inverseDistanceWeighted(distanceTemperatureCombinations: Iterable[(Double, Double)], power: Int): Double = {
    val (weightedSum, inverseWeightedSum) = distanceTemperatureCombinations
      .aggregate((0.0, 0.0))(
        {
          case ((ws, iws), (distance, temp)) => {
            val w = 1 / pow(distance, power)
            (w * temp + ws, w + iws)
          }
        }, {
          case ((wsA, iwsA), (wsB, iwsB)) => (wsA + wsB, iwsA + iwsB)
        }
      )

    weightedSum / inverseWeightedSum
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

  def isZero(distance: Double): Boolean =
    distance < 1d

  def getLocation(x: Int, y: Int): Location = {
    val lat = ImageCenterY - y
    val lon = x - ImageCenterX
    Location(lat, lon)
  }
}

