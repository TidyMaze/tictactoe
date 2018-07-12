import TicTacToe.Grid

sealed abstract class Player(val symbol: String)
case object PlayerX extends Player("X")
case object PlayerY extends Player("Y")


object TicTacToe extends App {
  type Grid = Seq[Seq[Option[Player]]]
  case class Coord(x: Int, y: Int)

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

  val Dim = 3
  val grid = mkGrid(Dim)
  printGrid(grid)
  println()
  printGrid(putPlayer(grid, PlayerX, Coord(0,0)))

  val winningGrid = Seq(Coord(0,0), Coord(0,1), Coord(0,2)).foldLeft(grid)((grid, coord) => putPlayer(grid, PlayerX, coord))
  printGrid(winningGrid)

}
