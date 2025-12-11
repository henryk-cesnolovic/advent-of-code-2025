import scala.io.Source
import Utils._

object Day11:
  @main
  def d11() =
    val data =
      Source.fromFile("d11/data.txt").getLines().to(List)
    val dataTest =
      Source.fromFile("d11/data_test.txt").getLines().to(List)
    val dataTest2 =
      Source.fromFile("d11/data_test2.txt").getLines().to(List)

    time(solution1(dataTest))
    time(solution1(data))

    time(solution2(dataTest2))
    time(solution2(data))

  def solution1(data: List[String]): Long =
    val ioMap = data.map { line =>
      val Array(inData, outData) = line.split(":")

      val outs = outData.trim.split(" ").toList

      inData -> outs
    }.toMap

    def loop(listToLook: List[String]): Int =
      if (listToLook.head == "out")
        1
      else
        listToLook.map(value => loop(ioMap(value))).sum

    loop(ioMap("you"))

  def solution2(data: List[String]): Long =
    val ioMap = data.map { line =>
      val Array(inData, outData) = line.split(":")

      val outs = outData.trim.split(" ").toList

      inData -> outs
    }.toMap

    def loop(start: String, end: String): Long =
      val cache = scala.collection.mutable.HashMap[(String, String), Long]()

      def loopInner(next: String): Long =
        if (!cache.contains((next, end)))
          if (next == end)
            1L
          else {
            var totalPaths = ioMap.get(next).map(_.foldLeft(0L) { (acc, n) => acc + loopInner(n) }).getOrElse(0L)
            cache += (next, end) -> totalPaths
            totalPaths
          }
        else cache((next, end))

      loopInner(start)

    val startToFft = loop("svr", "fft")
    val startToDac = loop("svr", "dac")

    val fftToDac = loop("fft", "dac")
    val dacToFft = loop("dac", "fft")

    val fftToOut = loop("fft", "out")
    val dacToOut = loop("dac", "out")

    (startToFft * fftToDac * dacToOut) + (startToDac * dacToFft * fftToOut)
