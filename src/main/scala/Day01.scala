import scala.io.Source
import scala.util.Using

object Day01 extends App {

  def readLinesFromResource(resourceName: String): List[Int] =
    Using(Source.fromResource(resourceName))(
      _
        .getLines()
        .map(_.toIntOption)
        .filter(_.isDefined)
        .map(_.get)
        .toList
    ).get

  def getWindows(xs: List[Int], windowSize: Int): List[List[Int]] =
    xs
      .drop(windowSize)
      .foldLeft(List(xs.take(windowSize)))((acc, x) => {
        (acc.head.tail ::: List(x)) :: acc
      })
      .reverse

  def countIncreases(xs: List[Int], windowSize: Int): Int = {
    val windows: List[List[Int]] = getWindows(xs, windowSize)

    windows.tail
      .zip(windows)
      .foldLeft(0)((acc, zipped) => {
        if (zipped._1.sum > zipped._2.sum) acc + 1
        else acc
      })
  }

  val input: List[Int] = readLinesFromResource("Day01.input.txt")
  println("Task 1 solution:")
  println(countIncreases(input, 1))
  println("Task 2 solution:")
  println(countIncreases(input, 3))

}
