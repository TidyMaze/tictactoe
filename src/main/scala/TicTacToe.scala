import TicTacToe.Grid

import scala.io.StdIn

sealed abstract class Player(val symbol: String)
case object PlayerX extends Player("X")
case object PlayerY extends Player("Y")


object TicTacToe extends App {
  type Grid = Seq[Seq[Option[Player]]]
  case class Coord(x: Int, y: Int)
  val Dim = 3

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
    case g if g(0)(0) == g(0)(1) && g(0)(1) == g(0)(2) => g(0)(0)
    case g if g(1)(0) == g(1)(1) && g(1)(1) == g(1)(2) => g(1)(0)
    case g if g(2)(0) == g(2)(1) && g(2)(1) == g(2)(2) => g(2)(0)
      // columns
    case g if g(0)(0) == g(1)(0) && g(1)(0) == g(2)(0) => g(0)(0)
    case g if g(0)(1) == g(1)(1) && g(1)(1) == g(2)(1) => g(0)(1)
    case g if g(0)(2) == g(1)(2) && g(1)(2) == g(2)(2) => g(0)(2)
      // diags
    case g if g(0)(0) == g(1)(1) && g(1)(1) == g(2)(2) => g(0)(0)
    case g if g(0)(2) == g(1)(1) && g(1)(1) == g(2)(0) => g(0)(2)
  }

  def getPlayerAction(player: Player): Coord = {
    val xy = StdIn.readLine(s"Action for player ${player.symbol}? ").split("").map(_.toInt)
    Coord(xy(0), xy(1))
  }

  def nextPlayer: Player => Player = {
    case PlayerX => PlayerY
    case PlayerY => PlayerX
  }

  def availableCells(grid: Grid): Seq[Coord] = for {
    x <- grid.head.indices
    y <- grid.indices
    if grid(y)(x).isEmpty
  } yield Coord(x,y)

  def stepOneTurn(currentPlayer: Player, currentGrid: Grid): Option[Player] = {
    println("Current grid:")
    printGrid(currentGrid)
    val action = getPlayerAction(currentPlayer)
    val nextGrid = putPlayer(currentGrid, currentPlayer, action)
    val maybeWinner = getWinner(nextGrid)
    val stillPlayable = availableCells(nextGrid).nonEmpty
    (maybeWinner, stillPlayable) match {
      case (Some(winner), _) => Some(winner)
      case (None, false) => None
      case (None, true) => stepOneTurn(nextPlayer(currentPlayer), nextGrid)
    }
  }

  def fullGame(): Option[Player] = stepOneTurn(PlayerX, mkGrid(Dim))

  val maybeWinner = fullGame()
  val endingInfo = maybeWinner.map(p => s"${p.symbol} WINS!").getOrElse("IT'S A DRAW!")
  println(endingInfo)
}
