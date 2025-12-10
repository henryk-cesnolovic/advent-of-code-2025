import scala.io.Source
import Utils._

object Day10:
  @main
  def d10() =
    val data =
      Source.fromFile("d10/data.txt").getLines().to(List)
    val dataTest =
      Source.fromFile("d10/data_test.txt").getLines().to(List)

    time(solution1(dataTest))
    time(solution1(data))

    // time(solution2(dataTest))
    // time(solution2(data))

  def solution1(data: List[String]): Long =
    data.map { line =>

      val split = line.split(" ").toList

      val desiredString = split.head.tail.dropRight(1)
      val switchesString = split.tail.dropRight(1)
      val joltages = split.last

      val desiredBitmask = desiredString
        .foldLeft((0L, 0)) { case ((acc, index), char) =>
          if (char == '#')
            (acc | (1 << index), index + 1)
          else (acc, index + 1)
        }
        ._1

      val switchesMasks = switchesString.map { switches =>
        switches.tail.dropRight(1).split(",").foldLeft(0L) { case (acc, switch) =>
          acc | (1 << switch.toInt)
        }
      }.toSet

      @scala.annotation.tailrec
      def findDesired(accSetMasks: Set[Long], accIteration: Int): Int =
        val newSetMasks = accSetMasks.flatMap { mask =>
          switchesMasks.map { switchesMask =>
            switchesMask ^ mask
          }
        }

        if (newSetMasks.contains(desiredBitmask))
          accIteration + 1
        else
          findDesired(newSetMasks, accIteration + 1)

      findDesired(Set(0L), 0)
    }.sum

  def solution2(data: List[String]): Long = ???
