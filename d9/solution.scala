import scala.io.Source
import Utils._

object Day9:
  @main
  def d9() =
    val data =
      Source.fromFile("d9/data.txt").getLines().to(List)
    val dataTest =
      Source.fromFile("d9/data_test.txt").getLines().to(List)

    time(solution1(dataTest))
    time(solution1(data))

    time(solution2(dataTest))
    time(solution2(data))

  case class Coordinates(x: Long, y: Long)

  def solution1(data: List[String]): Long =
    val coordinates = data.map(_.split(",")).map { case Array(x, y) => Coordinates(x.toLong, y.toLong) }
    val pairs = coordinates.combinations(2)

    val squares = pairs.map { case a :: b :: Nil => (Math.abs(a.x - b.x) + 1) * (Math.abs(a.y - b.y) + 1) }.toList.sortWith(_ > _)

    squares.head

  def solution2(data: List[String]): Long =
    val coordinates = data.map(_.split(",")).map { case Array(x, y) => Coordinates(x.toLong, y.toLong) }
    val pairs = coordinates.combinations(2).toList

    val walls = pairs
      .filter { case a :: b :: Nil => a.x == b.x || a.y == b.y }

    case class SquareWithPair(area: Long, pair: (Coordinates, Coordinates))
    val squaresWithPairs = pairs
      .map { case a :: b :: Nil =>
        SquareWithPair((Math.abs(a.x - b.x) + 1) * (Math.abs(a.y - b.y) + 1), (a, b))
      }
      .sortWith(_.area > _.area)

    @scala.annotation.tailrec
    def findRequiredMax(squaresWithPairsLeft: List[SquareWithPair]): Long =
      squaresWithPairsLeft match
        case Nil => throw new Exception("not found")
        case head :: tail =>
          val (a, b) = head.pair
          val (minX, maxX) = if (a.x < b.x) (a.x, b.x) else (b.x, a.x)
          val (minY, maxY) = if (a.y < b.y) (a.y, b.y) else (b.y, a.y)

          val foundWallInRec = walls.filter { wall =>
            val clip = wall
              .map { point =>

                var pos = 0
                if (point.y >= maxY) pos |= 8
                else if (point.y <= minY) pos |= 4

                if (point.x >= maxX) pos |= 2
                if (point.x <= minX) pos |= 1

                pos
              }

            clip.reduce(_ & _) == 0
          }

          if (foundWallInRec.isEmpty)
            head.area
          else
            findRequiredMax(tail)

    findRequiredMax(squaresWithPairs)
