import scala.io.Source
import Utils._
import scala.annotation.init

object Day10:
  @main
  def d10() =
    val data =
      Source.fromFile("d10/data.txt").getLines().to(List)
    val dataTest =
      Source.fromFile("d10/data_test.txt").getLines().to(List)

    time(solution1(dataTest))
    time(solution1(data))

    time(solution2(dataTest))
    time(solution2(data))

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

  def solution2(data: List[String]): Long =
    data.map { line =>

      val split = line.split(" ").toList

      val desiredString = split.head.tail.dropRight(1)
      val switchesString = split.tail.dropRight(1)
      val joltages = split.last.tail.dropRight(1).split(",").map(_.toInt)

      val switchesMasks = switchesString.map { switches =>
        switches.tail.dropRight(1).split(",").foldLeft(0L) { case (acc, switch) =>
          acc | (1 << switch.toInt)
        }
      }.toSet

      case class State(possibleJoltages: Array[Int], presses: List[Int], multipliers: List[Int])
      val resultStates = new scala.collection.mutable.HashSet[State]()
      val mem = new scala.collection.mutable.HashMap[Long, List[List[Long]]]()

      def findAllSolution(currentStates: Set[State]): Unit =
        if (!currentStates.isEmpty)
          val newStates: Set[State] = currentStates.flatMap { case State(joltages, presses, multipliers) =>
            val multiplier = if (joltages.filter(_ % 2 != 0).isEmpty) 2 else 1
            if (joltages.filter(_ % 2 != 0).isEmpty) {
              (0 to joltages.size - 1).map { index => joltages(index) /= 2 }
            }

            // joltages bitmask
            val initBitmask = joltages.reverse.foldLeft(0L) { (acc, joltage) =>
              (acc << 1) | (joltage % 2)
            }

            val allSolutions = mem.get(initBitmask) match {
              case Some(value) => value
              case None =>
                val subsets = (0 to switchesMasks.size).flatMap(switchesMasks.toList.combinations).toList
                val solutions = subsets
                  .map { subset =>
                    (subset.foldLeft(initBitmask) { case (acc, switch) => acc ^ switch }, subset)
                  }
                  .filter(_._1 == 0L)
                  .map(_._2)
                mem += initBitmask -> solutions
                solutions
            }

            val states = allSolutions.map { solution =>
              val joltagesCopy = joltages.clone()
              solution.foreach { bitmask =>
                var mutBitmask = bitmask
                var index = 0
                while (mutBitmask != 0) {
                  if ((mutBitmask & 1) == 1)
                    joltagesCopy(index) -= 1
                  index += 1
                  mutBitmask >>= 1
                }
              }
              State(joltagesCopy, solution.size +: presses, multiplier +: multipliers)
            }

            states.filter { state =>
              val joltages = state.possibleJoltages

              if (joltages.filter(_ != 0).isEmpty) {
                resultStates.add(state)
                false
              } else if (!joltages.filter(_ < 0).isEmpty)
                false
              else
                true
            }
          }
          findAllSolution(newStates)
        else ()

      findAllSolution(Set(State(joltages, List(), List())))

      val numOfPresses = resultStates.map { case State(_, presses, multiplier) =>
        var sumOfPresses = 0
        (0 to presses.size - 1).foreach { index =>
          sumOfPresses = (sumOfPresses + presses(index)) * multiplier(index)
        }
        sumOfPresses
      }

      numOfPresses.min
    }.sum
