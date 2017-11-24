package observatory


import org.scalatest.prop.Checkers
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

trait VisualizationTest extends FunSuite with Checkers {

  test("predicted temperature at location z should " +
    "be closer to known temperature at location x than " +
    "to known temperature at location y, if z is closer " +
    "(in distance) to x than y, and vice versa") {

    val x = (Location(90, 10), 10d)
    val y = (Location(-90, 10), 20d)

    val z = Location(89, 10) // closer to x

    val temp = Visualization.predictTemperature(Seq(x, y), z)

    assert( temp < (x._2 + x._2)/2, "z's temperature isn't closer to x's temperature" )

  }

}
