import scala.io.Source
import Utils._

object Day5:

  @main
  def d5() =
    val data =
      Source.fromFile("d5/data.txt").getLines().to(List)
    val dataTest =
      Source.fromFile("d5/data_test.txt").getLines().to(List)

    time(solution1(dataTest))
    time(solution1(data))

    time(solution2(dataTest))
    time(solution2(data))

  // check each ingridient if it falls to atleast one range
  def solution1(data: List[String]): Long =
    val splitIndex = data.lastIndexOf("")
    val twoParts = data.splitAt(splitIndex)

    val ranges =
      twoParts._1.map(_.split("-")).map(row => (row(0).toLong, row(1).toLong))
    val ingridients = twoParts._2.drop(1).map(_.toLong)

    ingridients.foldLeft(0L) { case (acc, ingridient) =>
      if (
        ranges.filter { range =>
          ingridient >= range._1 && ingridient <= range._2
        }.size > 0
      )
        acc + 1
      else
        acc
    }

  // try merge overlap ranges and sum differences between range end and start
  def solution2(data: List[String]): Long =
    val splitIndex = data.lastIndexOf("")
    val twoParts = data.splitAt(splitIndex)

    val ranges =
      twoParts._1.map(_.split("-")).map(row => (row(0).toLong, row(1).toLong))

    case class Point(value: Long, state: Int) // 0 - start, 1 - end
    val sorted = ranges
      .map { range =>
        Array(Point(range._1, 0), Point(range._2, 1))
      }
      .flatten
      .sortBy { case Point(v, s) => (v, s) }

    case class State(
        mergedRangeStart: Long,
        numOfRangesInMerge: Int,
        sumOfIngridients: Long
    )
    val result = sorted.foldLeft(State(0L, 0, 0L)) { case (acc, point) =>
      point.state.match {
        case 0 =>
          State(
            if (acc.numOfRangesInMerge == 0L) point.value else acc.mergedRangeStart,
            acc.numOfRangesInMerge + 1,
            acc.sumOfIngridients
          )
        case 1 =>
          State(
            acc.mergedRangeStart,
            acc.numOfRangesInMerge - 1,
            if (acc.numOfRangesInMerge - 1 == 0) acc.sumOfIngridients + point.value - acc.mergedRangeStart + 1 else acc.sumOfIngridients
          )
      }
    }

    result.sumOfIngridients
