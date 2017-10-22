package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row}
import org.apache.spark.sql.types.{DoubleType, StringType, StructField, StructType}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import timeusage.TimeUsage.{spark, _}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .config("spark.master", "local")
      .getOrCreate()


  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  test("timeUsageGrouped && timeUsageGroupedSql && timeUsageGroupedTyped") {

    val (columns, initDf) = read("/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val finalDf = timeUsageGrouped(summaryDf)
    val finalSqlDf = timeUsageGroupedSql(summaryDf)
    val finalDs = timeUsageGroupedTyped( timeUsageSummaryTyped(summaryDf) )

    val finalDfArr = finalDf.collect()
    val finalSqlDfArr = finalSqlDf.collect()
    val finalDsArr = finalDs.collect()

    assert( finalDfArr.sameElements(finalSqlDfArr), " finalDfArr.sameElements(finalSqlDfArr) was false " )
    assert( finalDfArr.sameElements(finalDsArr), "finalDfArr.sameElements(finalDsArr) was false" )
    assert( finalSqlDfArr.sameElements(finalDsArr), "finalSqlDfArr.sameElements(finalDsArr) was false" )

  }

}
