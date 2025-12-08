import scala.io.Source
import Utils._

object Day2:
  @main
  def d2() =
    val data =
      Source.fromFile("d2/data.txt").getLines().next().split(",").to(LazyList)
    val dataTest =
      Source.fromFile("d2/data_test.txt").getLines().next().split(",").to(LazyList)

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
        val Array(lower, upper) = line.split("-")

        var half = if (lower.size < 2) 1L else lower.substring(0, lower.length() / 2).toLong
        while (s"${half}${half}".toLong <= upper.toLong) {
          if (s"$half$half".toLong >= lower.toLong)
            newSum += s"$half$half".toLong
          half += 1
        }
        solution1(rest, newSum)
    }

  @annotation.tailrec
  def solution2(data: LazyList[String], sum: Long = 0L): Long =
    var newSum = sum
    data match {
      case LazyList() => sum
      case line #:: rest =>
        val numbers = line.split("-")
        val lower = numbers(0).toLong
        val upper = numbers(1).toLong

        var s = scala.collection.mutable.Set[Long]()
        var i = "1"
        while (s"$i$i".toLong <= upper) {
          var j = i + i
          while (j.toLong <= upper) {
            if (j.toLong >= lower)
              s.add(j.toLong)
            j += i
          }
          i = (i.toLong + 1).toString
        }
        val newSum = if (s.isEmpty) 0 else s.reduce(_ + _)
        solution2(rest, newSum + sum)
    }
