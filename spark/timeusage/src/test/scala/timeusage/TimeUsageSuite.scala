package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row, SparkSession}
import org.apache.spark.sql.types.{DoubleType, StringType, StructField, StructType}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("BTM test Time Usage")
      .config("spark.master", "local")
      .getOrCreate()

  val (columns, initDf) = TimeUsage.read("/timeusage/atussum.csv")

  test("headerColumns size") {
    //val rdd = spark.sparkContext.textFile(TimeUsage.fsPath("/timeusage/atussum.csv"))
    //val headerColumns = rdd.first().split(",").to[List]
    // Compute the schema based on the first line of the CSV file
    //val schema = TimeUsage.dfSchema(headerColumns)
    //schema.printTreeString()

    //val (columns, initDf) = TimeUsage.read("/timeusage/atussum.csv")
    val n = columns.size
    println(n)
    assert(n == 455, "ncols is 455")
  }

  test("row") {
    val line = List("20030100013280","1","-1","44")
    val r = TimeUsage.row(line)
    assert(true)
  }

  test("classifiedColumns") {
    val (primary, working, leisure) = TimeUsage.classifiedColumns(columns)
    assert(primary.size == 55, "primary does not have 55 column")
    assert(working.size == 23, "working does not have 23 column")
    assert(leisure.size == 346, "leisure does not have 346 column")
  }

  test("timeUsageSummary") {
    val (primaryNeedsColumns, workColumns, otherColumns) = TimeUsage.classifiedColumns(columns)
    val summaryDf = TimeUsage.timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    //val v = summaryDf.collect()
    //v.foreach(println)
    summaryDf.show()
    assert(true)
  }

}
