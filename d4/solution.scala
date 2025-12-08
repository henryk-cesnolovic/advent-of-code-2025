import scala.io.Source
import util.Try
import Utils._

object Day4:
  @main
  def d4() =
    val data =
      Source.fromFile("d4/data.txt").getLines().to(Array).map(_.toCharArray)

    val dataTest =
      Source.fromFile("d4/data_test.txt").getLines().to(Array).map(_.toCharArray)

    time(solution1(dataTest))
    time(solution1(data))

    time(solution2(dataTest))
    time(solution2(data))

  def solution1(data: Array[Array[Char]]): Long =
    var result = 0L

    var i = 0
    while (i < data.size) {

      var j = 0
      while (j < data(i).size) {

        if (data(i)(j) == '@') {
          var n = 0
          Try { if (data(i - 1)(j - 1) == '@') n += 1 }
          Try { if (data(i - 1)(j) == '@') n += 1 }
          Try { if (data(i - 1)(j + 1) == '@') n += 1 }
          Try { if (data(i)(j - 1) == '@') n += 1 }
          Try { if (data(i)(j + 1) == '@') n += 1 }
          Try { if (data(i + 1)(j - 1) == '@') n += 1 }
          Try { if (data(i + 1)(j) == '@') n += 1 }
          Try { if (data(i + 1)(j + 1) == '@') n += 1 }

          if (n < 4) result += 1
        }
        j += 1
      }
      i += 1
    }

    result

  def solution2(data: Array[Array[Char]], sum: Long = 0L): Long =
    var newArr = data
    var result = 0L

    var i = 0
    while (i < data.size) {
      var j = 0
      while (j < data(i).size) {

        if (data(i)(j) == '@') {
          var n = 0
          Try { if (data(i - 1)(j - 1) == '@') n += 1 }
          Try { if (data(i - 1)(j) == '@') n += 1 }
          Try { if (data(i - 1)(j + 1) == '@') n += 1 }
          Try { if (data(i)(j - 1) == '@') n += 1 }
          Try { if (data(i)(j + 1) == '@') n += 1 }
          Try { if (data(i + 1)(j - 1) == '@') n += 1 }
          Try { if (data(i + 1)(j) == '@') n += 1 }
          Try { if (data(i + 1)(j + 1) == '@') n += 1 }

          if (n < 4) {
            newArr(i)(j) = '.'
            result += 1
          }
        }
        j += 1
      }
      i += 1
    }

    if (result == 0)
      sum
    else
      solution2(newArr, result + sum)
