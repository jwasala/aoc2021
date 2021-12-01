package week1

import scala.io.Source
import scala.util.Using
import scala.language.postfixOps

object Day1 extends App {

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
  println(countIncreases(readLinesFromResource("Day1.input.txt")))
}
