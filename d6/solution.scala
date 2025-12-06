import scala.io.Source
import Utils._

object Day6:
  @main
  def d6() =
    val data =
      Source.fromFile("d6/data.txt").getLines().to(List)
    val dataTest =
      Source.fromFile("d6/data_test.txt").getLines().to(List)

    time(solution1(dataTest))
    time(solution1(data))

    time(solution2(dataTest))
    time(solution2(data))

  def solution1(data: List[String]): Long =
    val splited = data.map(_.trim.split("\\s+").toList)

    val numbers = splited.dropRight(1).map(row => row.map(num => num.toLong))
    val actions = splited.last

    case class State(index: Int, totalSum: Long)
    val result = actions.foldLeft(State(0, 0L)) { case (acc, sign) =>
      val requiredNumbers = numbers.map(_(acc.index))

      val sum = if (sign == "*") requiredNumbers.reduce(_ * _) else requiredNumbers.reduce(_ + _)
      State(acc.index + 1, acc.totalSum + sum)
    }
    result.totalSum

  def solution2(data: List[String]): Long =
    val splited = data.map(_.trim.split("\\s+").toList)

    val numbers = splited.dropRight(1)
    val actions = splited.last

    case class ColumnState(index: Int, columnSizes: List[Int])
    val columnSizes = actions
      .foldLeft(ColumnState(0, List())) { case (acc, sign) =>
        val requiredNumbers = numbers.map(_(acc.index))
        val longestNumber = requiredNumbers.map(_.length).max

        ColumnState(acc.index + 1, longestNumber +: acc.columnSizes)
      }
      .columnSizes
      .reverse

    case class ResultState(index: Int, position: Int, totalSum: Long)
    val dataWithoutSigns = data.dropRight(1)
    val result = actions.foldLeft(ResultState(0, 0, 0L)) { case (acc, sign) =>
      val calculatedNumbers = (0 to columnSizes(acc.index) - 1)
        .map { pos =>
          (0 to dataWithoutSigns.size - 1).foldLeft("") { case (accN, i) =>
            accN + dataWithoutSigns(i)(acc.position + pos)
          }
        }
        .map(_.trim.toLong)

      val sum = if (sign == "*") calculatedNumbers.reduce(_ * _) else calculatedNumbers.reduce(_ + _)

      ResultState(acc.index + 1, acc.position + columnSizes(acc.index) + 1, acc.totalSum + sum)

    }

    result.totalSum
