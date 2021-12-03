import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03 extends App {

  def decodeGammaRateFromResource(resourceName: String, width: Int): String =
    Using(Source.fromResource(resourceName))(
      _
        .getLines()
        .foldLeft(List.fill(width)(0))((acc, line) => {
          acc
            .zip(line)
            .map(t => if (t._2 == '1') t._1 + 1 else t._1 - 1)
        })
        .map(digit => if (digit >= 0) '1' else '0')
        .mkString
    ).get

  def decodeEpsilonRateFromGammaRate(gammaRate: String): String =
    gammaRate.map(digit => {
      if (digit == '0') '1' else '0'
    })

  println("Task 1 solution:")
  val input: String = "Day03.input.txt"
  val width: Int = Using(Source.fromResource(input))(_.getLines().next().length).get
  val gammaRate = decodeGammaRateFromResource(input, width)
  val epsilonRate = decodeEpsilonRateFromGammaRate(gammaRate)
  println(Integer.parseInt(gammaRate, 2) * Integer.parseInt(epsilonRate, 2))

  def getMorePopularDigit(line: List[Char]): Char = {
    val onesRank: Int = line.foldLeft(0)((acc, ch) => acc + (if (ch == '1') 1 else -1))
    if (onesRank >= 0) '1' else '0'
  }

  @tailrec
  def decodeIndexFromPopularityMethod(linesWithIndex: List[(String, Int)], mostPopular: Boolean): Int =
    linesWithIndex match {
      case (_, index) :: Nil => index
      case xs =>
        decodeIndexFromPopularityMethod(
          xs.filter(t => {
            if (mostPopular) t._1.head == getMorePopularDigit(xs.map(_._1).transpose.head)
            else t._1.head != getMorePopularDigit(xs.map(_._1).transpose.head)
          }).map(t => (t._1.tail, t._2)),
          mostPopular
        )
    }

  def getLinesFromResource(resourceName: String): List[String] =
    Using(Source.fromResource(resourceName))(resource => {
      resource
        .getLines()
        .toList
    }).get

  println("Task 2 solution:")
  val lines: List[String] = getLinesFromResource(input)
  val linesWithIndex: List[(String, Int)] = lines.zipWithIndex
  val oxygenGenIndex: Int = decodeIndexFromPopularityMethod(linesWithIndex, mostPopular = true)
  val co2Index: Int = decodeIndexFromPopularityMethod(linesWithIndex, mostPopular = false)
  val oxygenGen: String = lines(oxygenGenIndex)
  val co2: String = lines(co2Index)
  println(Integer.parseInt(oxygenGen, 2) * Integer.parseInt(co2, 2))
}