import scala.io.Source
import Utils._

object Day1:
  @main
  def d1() =
    val data =
      Source.fromFile("d1/data.txt").getLines().to(LazyList)
    val dataTest =
      Source.fromFile("d1/data_test.txt").getLines().to(LazyList)

    time(solution1(dataTest))
    time(solution1(data))

    time(solution2(dataTest))
    time(solution2(data))

  @annotation.tailrec
  def solution1(data: LazyList[String], dial: Int = 50, occ: Int = 0): Int =
    data match {
      case LazyList() => occ
      case line #:: rest =>
        val direction = line.head
        val num = line.tail.toInt

        val rotated = direction match {
          case 'L' => dial - num
          case 'R' => dial + num
        }

        val newDial = ((rotated % 100) + 100) % 100
        if (newDial == 0)
          solution1(rest, newDial, occ + 1)
        else
          solution1(rest, newDial, occ)
    }

  @annotation.tailrec
  def solution2(data: LazyList[String], dial: Int = 50, occ: Int = 0): Int =
    data match {
      case LazyList() => occ
      case line #:: rest =>
        val direction = line.head
        val num = line.tail.toInt

        val rotated = direction match {
          case 'L' => dial - num
          case 'R' => dial + num
        }

        val normaliseOccurance = if (dial == 0 && direction == 'L') -1 else 0
        val negativeOccurance = if (rotated < 0) 1 else 0
        val multipleOccurances = Math.abs(rotated / 100)
        val newDial = ((rotated % 100) + 100) % 100
        val zeroOccurance = if (newDial == 0 && multipleOccurances == 0) 1 else 0
        solution2(
          rest,
          newDial,
          occ + multipleOccurances + negativeOccurance + normaliseOccurance + zeroOccurance
        )
    }
