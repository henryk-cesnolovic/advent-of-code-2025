import scala.io.Source
import Utils._

object Day8:
  @main
  def d8() =
    val data =
      Source.fromFile("d8/data.txt").getLines().to(List)
    val dataTest =
      Source.fromFile("d8/data_test.txt").getLines().to(List)

    time(solution1(dataTest, 10))
    time(solution1(data, 1000))

    time(solution2(dataTest))
    time(solution2(data))

  case class Coordinates(x: Int, y: Int, z: Int):
    def distanceTo(that: Coordinates): Double =
      Math.sqrt(
        Math.pow(this.x - that.x, 2) +
          Math.pow(this.y - that.y, 2) +
          Math.pow(this.z - that.z, 2)
      )

  def solution1(data: List[String], numOfConnectios: Int): Long =
    val coordinates = data.map { line =>
      line.split(",").map(_.toInt) match
        case Array(x, y, z) => Coordinates(x, y, z)
    }

    case class DistanceWithPair(distance: Double, pair: (Coordinates, Coordinates))
    val distancesWithPairs = coordinates
      .combinations(2)
      .map { case first :: second :: Nil => DistanceWithPair(first.distanceTo(second), (first, second)) }
      .toList

    case class ResultState(circuits: List[Set[Coordinates]])
    distancesWithPairs
      .sortWith(_.distance < _.distance)
      .take(numOfConnectios)
      .foldLeft(ResultState(List())) { case (resultAcc, distanceWithPair) =>
        val (a, b) = distanceWithPair.pair

        case class DistanceState(notFoundCircuits: List[Set[Coordinates]], foundCircuits: List[Set[Coordinates]])
        val distanceState = resultAcc.circuits.foldLeft(DistanceState(List(), List())) { case (acc, circuit) =>
          if (circuit.contains(a))
            DistanceState(acc.notFoundCircuits, (circuit ++ Set(b)) +: acc.foundCircuits)
          else if (circuit.contains(b))
            DistanceState(acc.notFoundCircuits, (circuit ++ Set(a)) +: acc.foundCircuits)
          else
            DistanceState(circuit +: acc.notFoundCircuits, acc.foundCircuits)
        }

        if (distanceState.foundCircuits.isEmpty) ResultState(Set(a, b) +: resultAcc.circuits)
        else
          ResultState(distanceState.foundCircuits.reduce(_ ++ _) +: distanceState.notFoundCircuits)
      }
      .circuits
      .map(_.size.toLong)
      .sortWith(_ > _)
      .take(3)
      .product

  def solution2(data: List[String]): Long =
    val coordinates = data.map { line =>
      line.split(",").map(_.toInt) match
        case Array(x, y, z) => Coordinates(x, y, z)
    }

    case class DistanceWithPair(distance: Double, pair: (Coordinates, Coordinates))
    val distancesWithPairs = coordinates
      .combinations(2)
      .map { case first :: second :: Nil => DistanceWithPair(first.distanceTo(second), (first, second)) }
      .toList

    val numOfCoordinates = coordinates.size

    @scala.annotation.tailrec
    def findLastPairInFullCircuit(distancesWithPairsLeft: List[DistanceWithPair], circuits: List[Set[Coordinates]]): (Coordinates, Coordinates) =
      distancesWithPairsLeft match
        case Nil => throw new Exception("not found")
        case head :: tail =>
          val (a, b) = head.pair

          case class DistanceState(notFoundCircuits: List[Set[Coordinates]], foundCircuits: List[Set[Coordinates]])
          val distanceState = circuits.foldLeft(DistanceState(List(), List())) { case (acc, circuit) =>
            if (circuit.contains(a))
              DistanceState(acc.notFoundCircuits, (circuit ++ Set(b)) +: acc.foundCircuits)
            else if (circuit.contains(b))
              DistanceState(acc.notFoundCircuits, (circuit ++ Set(a)) +: acc.foundCircuits)
            else
              DistanceState(circuit +: acc.notFoundCircuits, acc.foundCircuits)
          }

          if (distanceState.foundCircuits.isEmpty) findLastPairInFullCircuit(tail, Set(a, b) +: circuits)
          else
            val mergedCircuits = distanceState.foundCircuits.reduce(_ ++ _)
            if (mergedCircuits.size == numOfCoordinates)
              (a, b)
            else
              findLastPairInFullCircuit(tail, mergedCircuits +: distanceState.notFoundCircuits)

    val pair = findLastPairInFullCircuit(distancesWithPairs.sortWith(_.distance < _.distance), List())
    pair._1.x * pair._2.x
