import scala.io.Source
import Utils._

object Day7:
  @main
  def d7() =
    val data =
      Source.fromFile("d7/data.txt").getLines().to(List)
    val dataTest =
      Source.fromFile("d7/data_test.txt").getLines().to(List)

    time(solution1(dataTest))
    time(solution1(data))

    time(solution2(dataTest))
    time(solution2(data))

  def solution1(data: List[String]): Long =
    val sPosition = data.head.indexOf("S")

    case class ResultState(beamPositions: Set[Int], totalSum: Long)
    case class LineState(newBeamsPositions: Set[Int], numberOfSplits: Long)
    data.tail
      .foldLeft(ResultState(Set(sPosition), 0L)) { case (resultStateAcc, line) =>
        val lineState = resultStateAcc.beamPositions.foldLeft(LineState(Set(), 0L)) { case (lineStateAcc, beamPosition) =>
          if (line(beamPosition) == '^')
            val newPositions = Set(beamPosition - 1, beamPosition + 1).filterNot(p => p < 0 || p > line.size - 1)
            LineState(lineStateAcc.newBeamsPositions ++ newPositions, lineStateAcc.numberOfSplits + 1)
          else LineState(lineStateAcc.newBeamsPositions ++ Set(beamPosition), lineStateAcc.numberOfSplits)
        }
        ResultState(lineState.newBeamsPositions, resultStateAcc.totalSum + lineState.numberOfSplits)
      }
      .totalSum

  def solution2(data: List[String]): Long =
    val sPosition = data.head.indexOf("S")
    val maxPosition = data.head.length()

    case class ResultState(beamPositionsWithTimelines: Map[Int, Long])
    case class LineState(newBeamsPositions: Map[Int, Long])
    data.tail
      .foldLeft(ResultState(Map(sPosition -> 1))) { case (resultAcc, line) =>
        val lineState = resultAcc.beamPositionsWithTimelines.keys.foldLeft(LineState(Map())) { case (lineStateAcc, beamPosition) =>
          val beamTimelines = resultAcc.beamPositionsWithTimelines(beamPosition)
          if (line(beamPosition) == '^')
            val newPositions = Set(beamPosition - 1, beamPosition + 1).filterNot(p => p < 0 || p > line.size - 1)
            val newPositionsWithTimelines = newPositions.map { newPosition =>
              val timelinesForNewPosition = lineStateAcc.newBeamsPositions
                .get(newPosition)
                .map(_ + beamTimelines)
                .getOrElse(beamTimelines)
              newPosition -> timelinesForNewPosition
            }.toMap
            LineState(lineStateAcc.newBeamsPositions ++ newPositionsWithTimelines)
          else
            val newPositionWithTimeLine = Map(
              beamPosition -> lineStateAcc.newBeamsPositions
                .get(beamPosition)
                .map(_ + beamTimelines)
                .getOrElse(beamTimelines)
            )
            LineState(lineStateAcc.newBeamsPositions ++ newPositionWithTimeLine)
        }
        ResultState(lineState.newBeamsPositions)
      }
      .beamPositionsWithTimelines
      .values
      .sum
