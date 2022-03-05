trait Color
case object Black extends Color
case object White extends Color

case class Point(x: Int, y: Int)

trait Checker
case class Piece(color: Color, point: Point) extends Checker
case object None extends Checker


case class Cell(point: Point, piece: Checker, color: Color)

case class GameBoard(cells: Vector[Cell])


object Board {

  def createBoard: Vector[Cell] = {
    Vector.range(1, 9).flatMap{
      r: Int => Vector.range(1,9).map{
          c: Int =>
            val curPoint = Point(r, c)
            val curColor = cellColor(r,c)
            hasPiece(r, c) match {
              case true => Cell(curPoint, Piece(pieceColor(r), curPoint), curColor)
              case false => Cell(curPoint, None, curColor)
            }
        }
    }
  }

  def hasPiece(row: Int, col: Int): Boolean = (row%2==0, col%2==0) match {
    case (true, false) if row<4||row>5 => true
    case (false, true) if row<4||row>5 => true
    case _ => false
  }

  def cellColor(row: Int, col: Int): Color = (row%2==0, col%2==0) match {
    case (false, true) => Black
    case (true, false) => Black
    case _ => White
  }

  def pieceColor(row: Int): Color =
    if(row<4) Black else White

  def printBoard(board: GameBoard): Unit = {
    val rows = board.cells.grouped(8).toVector

    for(row <- rows) {
      print(prettyPrintBox(row))
    }
  }

  def prettyPrintBox(row: Vector[Cell]): String = {
    var test = "\n"+"----"*20+"\n"
    for(cell <- row) {
      cell.piece match {
        case None => test += f"| ${"    "}%7s " 
        case Piece(color, Point(x, y)) => color match{
            case White => test += f"| ${"White"}%6s  "
            case Black => test += f"| ${"Black"}%6s  "
          }
      }
    }
    test += "\n"+"----"*20
    test
  }
}






