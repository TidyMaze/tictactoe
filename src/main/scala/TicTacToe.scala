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

  def getHumanAction: Brain = { (availableCells, grid, player) =>
    println("Current grid:")
    printGrid(grid)
    val xy = StdIn.readLine(s"Action for player ${player.symbol}? ").split("").map(_.toInt)
    val picked = Coord(xy(0), xy(1))
    if (availableCells.contains(picked)) {
      picked
    } else {
      println(s"Cell $picked is not allowed: Not available.")
      getHumanAction(availableCells, grid, player)
    }
  }

  def randomIn[A](xs: Seq[A]): A =
    if(xs.isEmpty)
      throw new InvalidParameterException("Empty seq")
     else
      xs(random.nextInt(xs.size))

  def getRandomAction: Brain = (availableCells, grid, player) => randomIn(availableCells)

  def nextPlayer: Player => Player = {
    case PlayerX => PlayerY
    case PlayerY => PlayerX
  }

  def availableCells(grid: Grid): Seq[Coord] = for {
    y <- grid.indices
    x <- grid.head.indices
    if grid(y)(x).isEmpty
  } yield Coord(x,y)

  def playTillEnd(currentPlayer: Player, currentGrid: Grid, getPlayerAction: Brain, getPlayerOppAction: Brain): (Option[Player], Grid) = {
    val possibleCells = availableCells(currentGrid)
    val action = getPlayerAction(possibleCells, currentGrid, currentPlayer)
    val nextGrid = putPlayer(currentGrid, currentPlayer, action)
    val maybeWinner = getWinner(nextGrid)
    val stillPlayable = availableCells(nextGrid).nonEmpty
    (maybeWinner, stillPlayable) match {
      case (Some(winner), _) => (Some(winner), nextGrid)
      case (None, false) => (None, nextGrid)
      case (None, true) => playTillEnd(nextPlayer(currentPlayer), nextGrid, getPlayerOppAction, getPlayerAction)
    }
  }

  def fullGame(player: Player, getPlayerAction: Brain, getPlayerOppAction: Brain, grid: Grid): (Option[Player], Grid) = playTillEnd(player, grid, getPlayerAction, getPlayerOppAction)

  def monteCarlo(currentPlayer: Player, grid: Grid, nbGames: Int): Coord = {
    val score: ((Coord, Seq[Option[Player]])) => Double = { entry =>
        val wins = entry._2.count(maybeWinner => maybeWinner.isDefined && maybeWinner.contains(currentPlayer))
        val loses = entry._2.count(maybeWinner => maybeWinner.isDefined && !maybeWinner.contains(currentPlayer))
        val draws = entry._2.count(maybeWinner => maybeWinner.isEmpty)
        (wins.toDouble * 1 + loses.toDouble * -1) / entry._2.size
    }
    availableCells(grid).map(rootAction => {
      val endings = 0 until nbGames map { _ =>
        fullGame(
          nextPlayer(currentPlayer),
          getRandomAction,
          getRandomAction,
          putPlayer(
            grid,
            currentPlayer,
            rootAction
          )
        )._1
      }
      rootAction -> endings
    }
    ).toMap.toSeq.maxBy(score)._1
  }

  def monteCarloBrain: Brain = (availableCells, grid, player) => monteCarlo(player, grid, 1000)

  val (maybeWinner, endingGrid) = fullGame(PlayerX, getHumanAction, monteCarloBrain, mkGrid(Dim))
  val endingInfo = maybeWinner.map(p => s"${p.symbol} WINS!").getOrElse("IT'S A DRAW!")
  println(endingInfo)
  printGrid(endingGrid)
}
