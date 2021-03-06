import java.security.InvalidParameterException

import TicTacToe.Brain

import scala.io.StdIn
import scala.util.Random

sealed abstract class Player(val symbol: String)
case object PlayerX extends Player("X")
case object PlayerY extends Player("Y")


object TicTacToe extends App {
  type Grid = Seq[Seq[Option[Player]]]
  type Brain = (Seq[Coord], Grid, Player) => Coord
  case class Coord(x: Int, y: Int)
  val Dim = 3

  val random = new Random()

  def showCell: Option[Player] => String = {
    case Some(p) => p.symbol
    case None => "-"
  }
  def showGrid(grid: Grid): String = grid.map(_.map(showCell).mkString(" ")).mkString("\n")
  def printGrid= showGrid _ andThen println
  def mkGrid(size: Int): Grid = Array.fill[Option[Player]](size,size)(None).map(_.toSeq).toSeq

  def putPlayer(grid: Grid, player: Player, at: Coord): Grid = grid.zipWithIndex.map {
    case (line, indexLine) if indexLine == at.y => line.take(at.x) ++ Seq(Some(player)) ++ line.drop(at.x + 1)
    case (line, _) => line
  }

  def getWinner(grid: Grid): Option[Player] = grid match {
      // rows
    case g if g(0)(0).isDefined && g(0)(0) == g(0)(1) && g(0)(1) == g(0)(2) => g(0)(0)
    case g if g(1)(0).isDefined && g(1)(0) == g(1)(1) && g(1)(1) == g(1)(2) => g(1)(0)
    case g if g(2)(0).isDefined && g(2)(0) == g(2)(1) && g(2)(1) == g(2)(2) => g(2)(0)
      // columns
    case g if g(0)(0).isDefined && g(0)(0) == g(1)(0) && g(1)(0) == g(2)(0) => g(0)(0)
    case g if g(0)(1).isDefined && g(0)(1) == g(1)(1) && g(1)(1) == g(2)(1) => g(0)(1)
    case g if g(0)(2).isDefined && g(0)(2) == g(1)(2) && g(1)(2) == g(2)(2) => g(0)(2)
      // diags
    case g if g(0)(0).isDefined && g(0)(0) == g(1)(1) && g(1)(1) == g(2)(2) => g(0)(0)
    case g if g(0)(2).isDefined && g(0)(2) == g(1)(1) && g(1)(1) == g(2)(0) => g(0)(2)
    case _ => None
  }

  def randomIn[A](xs: Seq[A]): A =
    if(xs.isEmpty)
      throw new InvalidParameterException("Empty seq")
     else
      xs(random.nextInt(xs.size))

  def nextPlayer: Player => Player = {
    case PlayerX => PlayerY
    case PlayerY => PlayerX
  }

  def availableCells(grid: Grid): Seq[Coord] = for {
    y <- grid.indices
    x <- grid.head.indices
    if grid(y)(x).isEmpty
  } yield Coord(x,y)

  def playTillEnd(currentPlayer: Player, currentGrid: Grid, currentBrain: Brain, oppBrain: Brain): (Option[Player], Grid) = {
    val maybePlayer = getWinner(currentGrid)
    val cells = availableCells(currentGrid)

    (maybePlayer, cells) match {
      case (Some(winner), _) => (Some(winner), currentGrid)
      case (None, Nil) => (None, currentGrid)
      case (None, _) =>
        playTillEnd(nextPlayer(currentPlayer), putPlayer(currentGrid, currentPlayer, currentBrain(cells, currentGrid, currentPlayer)), oppBrain, currentBrain)
    }
  }

  def monteCarlo(currentPlayer: Player, grid: Grid, nbGames: Int): Coord = {
    val score: ((Coord, Seq[Option[Player]])) => (Coord, Double) = { entry =>
        val wins = entry._2.count(maybeWinner => maybeWinner.isDefined && maybeWinner.contains(currentPlayer))
        val loses = entry._2.count(maybeWinner => maybeWinner.isDefined && !maybeWinner.contains(currentPlayer))
        entry._1 -> (wins.toDouble * 1 + loses.toDouble * -1) / entry._2.size
    }
    val sorted = availableCells(grid).map(rootAction => {
      val endings = 0 until nbGames map { _ =>
        val nextGrid = putPlayer(grid, currentPlayer, rootAction)
        val opp = nextPlayer(currentPlayer)
        playTillEnd(opp, nextGrid, getRandomAction, getRandomAction)._1
      }
      rootAction -> endings
    }).map(score).sortBy(-_._2)
    println("MonteCarlo eval: ")
    println(sorted.toSeq.map(c => c._1 + " -> " + c._2).mkString("\n"))
    println()
    sorted.head._1
  }

  def monteCarloBrain: Brain = (_, grid, player) => monteCarlo(player, grid, 1000)

  def getHumanAction: Brain = { (availableCells, grid, player) =>
    println("Current grid:")
    printGrid(grid)
    val raw = StdIn.readLine(s"Action for player ${player.symbol}? ")
    println()
    val picked = """^(\d)(\d)$""".r.findFirstMatchIn(raw)
      .map(m => Coord(m.group(1).toInt, m.group(2).toInt))
      .getOrElse {
        println(s"Invalid input, valid format is XY with X = column and Y = row. Starting at top left (0,0)")
        getHumanAction(availableCells, grid, player)
      }
    if (availableCells.contains(picked)) {
      picked
    } else {
      println(s"Cell $picked is not allowed: Not available.")
      getHumanAction(availableCells, grid, player)
    }
  }

  def getRandomAction: Brain = (availableCells, grid, player) => randomIn(availableCells)

  val (maybeWinner, endingGrid) = playTillEnd(PlayerX, mkGrid(Dim), monteCarloBrain, getHumanAction)
  val endingInfo = maybeWinner.map(p => s"${p.symbol} WINS!").getOrElse("IT'S A DRAW!")
  println(endingInfo)
  printGrid(endingGrid)
}
