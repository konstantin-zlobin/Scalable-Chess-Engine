package ru.zconstz.chess

import ru.zconstz.chess.Piece.MovementTemplate

object PieceColor extends Enumeration("White", "Black") {
  type PieceColor = Value
  val White, Black = Value

  implicit def char2PieceColor(char: Char): PieceColor.PieceColor = char match {
    case 'w' => PieceColor.White
    case 'b' => PieceColor.Black
    case _ => throw new IllegalArgumentException("valid values are 'w' and 'b'");
  }

  implicit def pieceColor2String(color: PieceColor.PieceColor): String = color match {
    case White => "w"
    case Black => "b"
  }
}


object Piece {

  case class Location(col: Char, row: Int)

  case class Movement(from: Location, to: Location)

  case class MovementTemplate(colDelta: Int, rowDelta: Int)

  implicit def tuple2Location(tuple: Tuple2[Char, Int]): Location = Location(tuple._1, tuple._2)

  implicit def tuple2Movement(tuple: Tuple2[Location, Location]): Movement = Movement(tuple._1, tuple._2)

  implicit def tuple2MovementTemplate(tuple: Tuple2[Int, Int]): MovementTemplate = MovementTemplate(tuple._1, tuple._2)

  implicit def str2Location(str: String): Location = Location(str.charAt(0), str(1) - '0')

  def diagonals(maxDist: Int = 7): Seq[Piece.MovementTemplate] = (1 to maxDist).map(n => List[Piece.MovementTemplate]((n, n), (-n, -n), (n, -n), (-n, n))).flatten

  def horizontalsAndVerticals(maxDist: Int = 7): Seq[Piece.MovementTemplate] = (1 to maxDist).map(n => List[Piece.MovementTemplate]((0, n), (0, -n), (n, 0), (-n, 0))).flatten

  def allStraitDirections(maxDist: Int = 7): Seq[Piece.MovementTemplate] = diagonals(maxDist) ++ horizontalsAndVerticals(maxDist)
}

abstract class Piece(val color: PieceColor.PieceColor, val weight: Int) {
  val movementTemplates: Seq[Piece.MovementTemplate]

}

//private trait ColorFactory[T] {
//  val white: T = apply(PieceColor.White)
//  val black: T = apply(PieceColor.Black)
//
//  def oppositeColor(color: PieceColor.PieceColor): T = color match {
//    case PieceColor.White => black
//    case PieceColor.Black => white
//  }
//
//  def sameColor(color: PieceColor.PieceColor): T = color match {
//    case PieceColor.White => white
//    case PieceColor.Black => black
//  }
//
//  def apply(color: PieceColor.PieceColor): T
//}

//object King extends ColorFactory[King]
//
//object Queen extends ColorFactory[Queen]
//
//object Rook extends ColorFactory[Rook]
//
//object Bishop extends ColorFactory[Bishop]
//
//object Knight extends ColorFactory[Knight]
//
//object Pawn extends ColorFactory[Pawn]

case class King(override val color: PieceColor.PieceColor) extends Piece(color, 10000) {
  override val movementTemplates = Piece.allStraitDirections()
}

case class Queen(override val color: PieceColor.PieceColor) extends Piece(color, 8) {
  override val movementTemplates = Piece.allStraitDirections()
}

case class Rook(override val color: PieceColor.PieceColor) extends Piece(color, 5) {
  override val movementTemplates = Piece.horizontalsAndVerticals()
}

case class Bishop(override val color: PieceColor.PieceColor) extends Piece(color, 3) {
  override val movementTemplates = Piece.diagonals()
}

case class Knight(override val color: PieceColor.PieceColor) extends Piece(color, 3) {
  override val movementTemplates = List(1, -1).combinations(2).map(multiplier =>
    List(1, 2).permutations.map(basement => MovementTemplate(basement(0) * multiplier(0), basement(1) * multiplier(1)))).flatten
    .toSeq
}

case class Pawn(override val color: PieceColor.PieceColor) extends Piece(color, 1) {
  override val movementTemplates = color match {
    case PieceColor.White => Seq[MovementTemplate]((0, 1), (0, 2), (1, 1), (-1, 1))
    case PieceColor.Black => Seq[MovementTemplate]((0, -1), (0, -2), (1, -1), (-1, -1))
  }
}
