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

    case class AllBeamsState(allBeamPositions: List[Set[Int]])
    case class LineState(newBeamsPositions: Set[Int])
    val allBeams = data.tail
      .foldLeft(AllBeamsState(List(Set(sPosition)))) { case (allBeamsStateAcc, line) =>
        val lineState = allBeamsStateAcc.allBeamPositions.head.foldLeft(LineState(Set())) { case (lineStateAcc, beamPosition) =>
          if (line(beamPosition) == '^')
            val newPositions = Set(beamPosition - 1, beamPosition + 1).filterNot(p => p < 0 || p > line.size - 1)
            LineState(lineStateAcc.newBeamsPositions ++ newPositions)
          else LineState(lineStateAcc.newBeamsPositions ++ Set(beamPosition))
        }
        AllBeamsState(lineState.newBeamsPositions +: allBeamsStateAcc.allBeamPositions)
      }

    @scala.annotation.tailrec
    def calcPaths(leftBeams: List[Set[Int]], accBeamsPaths: Map[Int, Long], lineIndex: Int): Long = // beams, numOfPaths
      leftBeams match {
        case Nil => accBeamsPaths.values.head
        case current :: tail =>
          val newBeamsPaths = accBeamsPaths.keys.foldLeft(Map[Int, Long]()) { case (acc, beam) =>
            val numOfPathsForBeam = accBeamsPaths(beam)

            acc ++
              (if (current.contains(beam))
                 acc ++ Map(beam -> acc.get(beam).map(_ + numOfPathsForBeam).getOrElse(numOfPathsForBeam))
               else Map()) ++
              (if (beam - 1 > -1 && data(lineIndex)(beam - 1) == '^')
                 Map(beam - 1 -> acc.get(beam - 1).map(_ + numOfPathsForBeam).getOrElse(numOfPathsForBeam))
               else Map()) ++
              (if (beam + 1 < maxPosition && data(lineIndex)(beam + 1) == '^')
                 Map(beam + 1 -> acc.get(beam + 1).map(_ + numOfPathsForBeam).getOrElse(numOfPathsForBeam))
               else Map())

          }
          calcPaths(tail, newBeamsPaths, lineIndex - 1)
      }

    calcPaths(allBeams.allBeamPositions.tail, allBeams.allBeamPositions.head.map(_ -> 1L).toMap, data.size - 1)
