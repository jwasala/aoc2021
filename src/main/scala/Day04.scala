import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day04 extends App {
  case class BoardItem(num: Int, marked: Boolean)

  type Board = List[List[BoardItem]]

  type Game = List[Board]

  extension (b: Board)
    def hasFullRow: Boolean = b.exists(_.forall(_.marked))
    def hasFullCol: Boolean = b.transpose.exists(_.forall(_.marked))
    def isWinning: Boolean = b.hasFullRow || b.hasFullCol

  extension (g: Game)
    def drawNumber(num: Int): Game =
      g.map(_.map(_.map(item =>
        if item.num == num
        then BoardItem(item.num, true)
        else item
      )))

    def drawNumbers(nums: List[Int], prevNum: Int = 0): (Int, Int) =
    // Returns tuple containing (winning board index, winning board score)
      g.indexWhere(_.isWinning) match {
        case -1 => g
          .drawNumber(nums.head)
          .drawNumbers(nums.tail, nums.head)
        case n => (n, g(n).flatten.filter(!_.marked).map(_.num).sum * prevNum)
      }

    def drawNumbersUntilLastWinner(nums: List[Int], prevNum: Int = 0, winnersIndices: List[Int] = Nil, lastWin: (Int, Int) = (0, 0)): (Int, Int) =
    // Returns tuple containing (winning board index, winning board score)
      if nums.isEmpty
      then lastWin
      else g
        .zipWithIndex
        .indexWhere((b, i) => !winnersIndices.contains(i) && b.isWinning) match {
        case -1 => g
          .drawNumber(nums.head)
          .drawNumbersUntilLastWinner(nums.tail, nums.head, winnersIndices, lastWin)
        case n => g
          .drawNumber(nums.head)
          .drawNumbersUntilLastWinner(
            nums,
            prevNum,
            n :: winnersIndices,
            (n, g(n).flatten.filter(!_.marked).map(_.num).sum * prevNum)
          )
      }

  def parseNumbers(line: String): List[Int] =
    line
      .split(',')
      .toList
      .map(_.toIntOption)
      .filter(_.isDefined)
      .map(_.get)

  def parseBoards(lines: List[String], size: Int): List[Board] =
    lines
      .map(
        _
          .split(" +")
          .map(_.toIntOption)
          .filter(_.isDefined)
          .map(num => BoardItem(num.get, false))
          .toList
      )
      .grouped(size)
      .toList

  def parseResource(resourceName: String, size: Int): (List[Int], Game) = {
    val lines: List[String] = Using(Source.fromResource(resourceName))(_.getLines().toList).get.filter(_ != "")
    (parseNumbers(lines.head), parseBoards(lines.tail, size))
  }

  val input: (List[Int], Game) = parseResource("Day04.input.txt", 5)
  val result1: (Int, Int) = input._2.drawNumbers(input._1)
  val result2: (Int, Int) = input._2.drawNumbersUntilLastWinner(input._1)
  println("Task 1 solution:")
  println(result1._1)
  println(result1._2)
  println("Task 2 solution:")
  println(result2._1)
  println(result2._2)
}
