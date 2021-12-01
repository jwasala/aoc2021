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

  def countIncreases(xs: List[Int]): Int =
    xs.tail
      .zip(xs)
      .foldLeft(0)((acc, zipped) => {
        if (zipped._1 > zipped._2) acc + 1
        else acc
      })

  println("Task 1 solution:")
  println(countIncreases(readLinesFromResource("Day01.input.txt")))
}
