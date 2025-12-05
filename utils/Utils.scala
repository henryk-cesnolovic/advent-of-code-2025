object Utils:
  def time[T](block: => T): T =
    val before = System.nanoTime
    val result = block
    val after = System.nanoTime
    println(s"Elapsed time: ${(after - before) / 1000000}ms. Result: $result")
    result
