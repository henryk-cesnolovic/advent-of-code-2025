import scala.io.Source
import Utils._

object Day3:
  @main
  def d3() =
    val data =
      Source.fromFile("d3/data.txt").getLines().to(LazyList)
    val dataTest =
      Source.fromFile("d3/data_test.txt").getLines().to(LazyList)

    time(solution1(dataTest))
    time(solution1(data))

    time(solution2(dataTest))
    time(solution2(data))

  @annotation.tailrec
  def solution1(data: LazyList[String], sum: Long = 0L): Long =
    var newSum = sum
    data match {
      case LazyList() => sum
      case line #:: rest =>
        var ch1 = '1'
        var ch2 = '1'

        var i = 0
        while (i < line.length() - 1) {
          if (ch1 < line(i))
            ch1 = line(i)
            ch2 = line(i + 1)
          else if (ch2 < line(i + 1))
            ch2 = line(i + 1)
          i += 1
        }

        solution1(rest, (s"$ch1$ch2".toLong) + sum)
    }

  @annotation.tailrec
  def solution2(data: LazyList[String], sum: Long = 0L): Long =
    var newSum = sum
    data match {
      case LazyList() => sum
      case line #:: rest =>
        var ch1 = '1'
        var ch2 = '1'
        var ch3 = '1'
        var ch4 = '1'
        var ch5 = '1'
        var ch6 = '1'
        var ch7 = '1'
        var ch8 = '1'
        var ch9 = '1'
        var ch10 = '1'
        var ch11 = '1'
        var ch12 = '1'

        var i = 0
        while (i < line.length() - 11) {
          if (ch1 < line(i))
            ch1 = line(i)
            ch2 = line(i + 1)
            ch3 = line(i + 2)
            ch4 = line(i + 3)
            ch5 = line(i + 4)
            ch6 = line(i + 5)
            ch7 = line(i + 6)
            ch8 = line(i + 7)
            ch9 = line(i + 8)
            ch10 = line(i + 9)
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch2 < line(i + 1))
            ch2 = line(i + 1)
            ch3 = line(i + 2)
            ch4 = line(i + 3)
            ch5 = line(i + 4)
            ch6 = line(i + 5)
            ch7 = line(i + 6)
            ch8 = line(i + 7)
            ch9 = line(i + 8)
            ch10 = line(i + 9)
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch3 < line(i + 2))
            ch3 = line(i + 2)
            ch4 = line(i + 3)
            ch5 = line(i + 4)
            ch6 = line(i + 5)
            ch7 = line(i + 6)
            ch8 = line(i + 7)
            ch9 = line(i + 8)
            ch10 = line(i + 9)
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch4 < line(i + 3))
            ch4 = line(i + 3)
            ch5 = line(i + 4)
            ch6 = line(i + 5)
            ch7 = line(i + 6)
            ch8 = line(i + 7)
            ch9 = line(i + 8)
            ch10 = line(i + 9)
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch5 < line(i + 4))
            ch5 = line(i + 4)
            ch6 = line(i + 5)
            ch7 = line(i + 6)
            ch8 = line(i + 7)
            ch9 = line(i + 8)
            ch10 = line(i + 9)
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch6 < line(i + 5))
            ch6 = line(i + 5)
            ch7 = line(i + 6)
            ch8 = line(i + 7)
            ch9 = line(i + 8)
            ch10 = line(i + 9)
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch7 < line(i + 6))
            ch7 = line(i + 6)
            ch8 = line(i + 7)
            ch9 = line(i + 8)
            ch10 = line(i + 9)
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch8 < line(i + 7))
            ch8 = line(i + 7)
            ch9 = line(i + 8)
            ch10 = line(i + 9)
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch9 < line(i + 8))
            ch9 = line(i + 8)
            ch10 = line(i + 9)
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch10 < line(i + 9))
            ch10 = line(i + 9)
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch11 < line(i + 10))
            ch11 = line(i + 10)
            ch12 = line(i + 11)
          else if (ch12 < line(i + 11))
            ch12 = line(i + 11)

          i += 1
        }

        solution2(
          rest,
          (s"$ch1$ch2$ch3$ch4$ch5$ch6$ch7$ch8$ch9$ch10$ch11$ch12".toLong) + sum
        )
    }
