import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

object Day02 extends App {
  abstract class Command(val units: Int)

  case class Forward(override val units: Int) extends Command(units)

  case class Down(override val units: Int) extends Command(units)

  case class Up(override val units: Int) extends Command(units)

  val command: Regex = raw"([A-z]+) ([0-9]+)".r

  def parseCommand(line: String): Option[Command] =
    line match {
      case command(name, units) if units.toIntOption.isDefined => name match {
        case "forward" => Some(Forward(units.toInt))
        case "down" => Some(Down(units.toInt))
        case "up" => Some(Up(units.toInt))
        case _ => None
      }
      case _ => None
    }

  def parseCommandsFromResource(resourceName: String): List[Command] =
    Using(Source.fromResource(resourceName))(
      _
        .getLines()
        .map(parseCommand)
        .filter(_.isDefined)
        .map(_.get)
        .toList
    ).get

  // Task 1
  {

    case class Position(depth: Int, horizontal: Int)

    def moveSubmarine(command: Command, p: Position): Position =
      command match {
        case Down(units) => Position(p.depth + units, p.horizontal)
        case Up(units) => Position(p.depth - units, p.horizontal)
        case Forward(units) => Position(p.depth, p.horizontal + units)
      }

    def moveSubmarineMultiple(commands: List[Command], startPosition: Position): Position =
      commands.foldLeft(startPosition)((currentPos, nextComm) => {
        moveSubmarine(nextComm, currentPos)
      })


    val input: List[Command] = parseCommandsFromResource("Day02.input.txt");
    println("Task 1 solution:")
    val finalPosition: Position = moveSubmarineMultiple(input, Position(0, 0))
    println(finalPosition.horizontal * finalPosition.depth)
  }

  // Task 2
  {

    case class Position(depth: Int, horizontal: Int, aim: Int)

    def moveSubmarine(command: Command, p: Position): Position =
      command match {
        case Down(units) => Position(p.depth, p.horizontal, p.aim + units)
        case Up(units) => Position(p.depth, p.horizontal, p.aim - units)
        case Forward(units) => Position(p.depth + (p.aim * units), p.horizontal + units, p.aim)
      }

    def moveSubmarineMultiple(commands: List[Command], startPos: Position): Position =
      commands.foldLeft(startPos)((currentPos, nextComm) => {
        moveSubmarine(nextComm, currentPos)
      })

    val input: List[Command] = parseCommandsFromResource("Day02.input.txt");
    println("Task 2 solution:")
    val finalPosition: Position = moveSubmarineMultiple(input, Position(0, 0, 0))
    println(finalPosition.horizontal * finalPosition.depth)
  }
}
