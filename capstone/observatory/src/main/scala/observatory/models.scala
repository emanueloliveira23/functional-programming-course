package observatory

import org.apache.commons.math3.util.FastMath._

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double)

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int) {
  def location: Location = {
    val n = pow(2d, zoom)
    val lon = ((x.toDouble / n) % 1.0) * 360d - 180d
    val lat = ((atan(sinh(PI * (1.0 - 2.0 * y / n))).toDegrees + 90) % 180.0) - 90
    Location(lat, lon)
  }
}

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int) {
  def location = Location(lat, lon)
}

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int)

//case class Point(ϕ: Double, λ: Double) {
//  lazy val location:Location = Location(toDegrees(ϕ), toDegrees(λ))
//
//  /**
//    * Added for special case: https://www.coursera.org/learn/scala-capstone/discussions/weeks/2/threads/YY0u6Ax8EeeqzRJFs29uDA
//    *
//    * @param other Point for distance calculatuion
//    * @return distance on earth in meters
//    */
//  def haversineEarthDistance(other: Point): Double = {
//    var r = 6372.8 // mean radius Earth in KM
//    r * greatCircleDistance(other) * 1000
//  }
//
//  /**
//    * https://en.wikipedia.org/wiki/Great-circle_distance#Computational_formulas
//    *
//    * @param other Point for distance calculatuion
//    * @return distance in radians
//    */
//  def greatCircleDistance(other: Point): Double = {
//    val Δϕ = abs(other.ϕ - ϕ)
//    val Δλ = abs(other.λ - λ)
//
//    val a =  pow(sin(Δϕ / 2), 2) + cos(ϕ) * cos(other.ϕ) * pow(sin(Δλ / 2), 2)
//    2 * atan2(sqrt(a), sqrt(1 - a))
//  }
//
//}