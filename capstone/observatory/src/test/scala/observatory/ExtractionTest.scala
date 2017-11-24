package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

trait ExtractionTest extends FunSuite {

  case class LocateTemperatureEntry(entry: (LocalDate, Location, Temperature)) {

    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)

    override def equals(o: scala.Any): Boolean = o match {
      case that: LocateTemperatureEntry =>
        this.entry._1 === that.entry._1 &&
        this.entry._2 === that.entry._2 &&
        doubleEquality.areEqual(this.entry._3, that.entry._3)
      case _ => super.equals(o)
    }
  }

  test("Test locateTemperatures as course site") {

    val result = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv");

    val truth = Seq(
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0),
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3)
    )

    assert(
      result
        .map(LocateTemperatureEntry)
        .toSet
        .sameElements(
          truth
            .map(LocateTemperatureEntry)
            .toSet
        )
    )
  }

}