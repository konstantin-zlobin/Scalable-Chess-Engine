package ru.zconstz.chess

import ru.zconstz.chess.Piece.Location

/**
 * @author Konstantin Zlobin
 */

abstract class Position {
  val pieces: Map[Piece.Location, Piece]

  def isCheck: Boolean = true //whatHappenToKings(isKingUnderAttackByPiece(_, _, _))

  def isMate: Boolean = false //whatHappenToKings((king, piece, movementTemplate) => isKingUnderAttackByPiece(king,
  //piece,
  //movementTemplate) && pieceLegalMovements(king).isEmpty)


  def apply(location: Piece.Location): Option[Piece] = pieces.get(location)
  //  def weight: Map[PieceColor.PieceColor, Int] =
  //    pieces.foldLeft(Map[PieceColor.PieceColor, Int](PieceColor.White -> 0, PieceColor.Black -> 0)) {
  //      (result, piece) => piece._2.color match {
  //        case PieceColor.White => Map(PieceColor.White -> (result(PieceColor.White) + piece._2.weight), PieceColor.Black -> result(PieceColor.Black))
  //        case PieceColor.Black => Map(PieceColor.White -> result(PieceColor.White), PieceColor.Black -> (result(PieceColor.Black) + piece._2.weight))
  //      }
  //    }
  //
  //  def locationByTemplate(location: Piece.Location, movementTemplate: Piece.MovementTemplate): Piece.Location =
  //    ((location._1 + movementTemplate._1).toChar, location._2 + movementTemplate._2)
  //
  //  def pieceLegalMovements(piece: Piece): Seq[Piece.Location] = {
  //    def isNotPossiblePawnMovement(movement: Piece.MovementTemplate): Boolean = movement._2 == 2 && piece.location._2 != 2 || movement._2 == -2 && piece.location._2 != 7
  //
  //    def isNotPossibleKingMovement(movement: Piece.MovementTemplate): Boolean = allFiguresOfOppositeColor(piece.color)
  //      .find(p => p.movementTemplates.find(t => locationByTemplate(p.location,
  //      t) == locationByTemplate(piece.location, movement)).isDefined).isDefined
  //
  //    def isOutOfBoardBoundaries(locationToCheck: Piece.Location) = locationToCheck._1 < 'a' || locationToCheck._1 > 'h' || locationToCheck._2 < 1 || locationToCheck._2 > 8
  //
  //    def placeIsBusyWithPieceOfSameColorSaveCastlingCase(locationToCheck: Piece.Location) = {
  //      pieces.get(locationToCheck) match {
  //        case None => false;
  //        case Some(pieceAtLocation) => pieceAtLocation.color == piece.color &&
  //          !(pieceAtLocation.isInstanceOf[King] && piece.isInstanceOf[Rook]) &&
  //          !(pieceAtLocation.isInstanceOf[Rook] && piece.isInstanceOf[King])
  //      }
  //    }
  //
  //    piece.possibleMovementTemplates
  //      .filterNot(movement => piece match {
  //      case Pawn(_, _) => isNotPossiblePawnMovement(movement)
  //      case King(_, _) => isNotPossibleKingMovement(movement)
  //      case _ => false
  //    }).map(movement => locationByTemplate(piece.location, movement))
  //      .filterNot(newPosition => isOutOfBoardBoundaries(newPosition) || placeIsBusyWithPieceOfSameColorSaveCastlingCase(newPosition))
  //  }

  //  def whatHappenToKings(predicate: (King, Piece, Piece.MovementTemplate) => Boolean): Boolean = {
  //    def kings = pieces.values.filter(p => p.isInstanceOf[King]).asInstanceOf[Iterable[King]]
  //    def findCheckMovementsOf(piece: Piece) = piece.movementTemplates.filter(movementTemplate => kings.find
  //      (predicate(_, piece, movementTemplate)).isDefined)
  //    pieces.values.find(piece => !findCheckMovementsOf(piece).isEmpty).isDefined
  //  }

  //  def isKingUnderAttackByPiece(king: King, piece: Piece, movementTemplate: Piece.MovementTemplate): Boolean = {
  //    king.location == locationByTemplate(piece.location, movementTemplate) && king.color != piece.color
  //  }

  //  def allFiguresOfColor(color: PieceColor.PieceColor): Iterable[Piece] = values.filter(_.color == color)
  //
  //  def allFiguresOfOppositeColor(color: PieceColor.PieceColor): Iterable[Piece] = values.filterNot(_.color == color)

  import ru.zconstz.chess.PieceColor.pieceColor2String

  override def toString(): String =
    (for (row <- (1 to 8)) yield (for (col <- ('a' to 'h')) yield (pieces.get(Location(col, row)) match {
      case Some(King(color)) => "K" + (color: String)
      case Some(Queen(color)) => "Q" + (color: String)
      case Some(Rook(color)) => "R" + (color: String)
      case Some(Bishop(color)) => "B" + (color: String)
      case Some(Knight(color)) => "N" + (color: String)
      case Some(Pawn(color)) => "P" + (color: String)
      case None => "  "
    })).mkString(row + "-[", ",", "]")).mkString(System.lineSeparator()) +
      System.lineSeparator() + ('A' to 'H').mkString("   ", "  ", "")

}

object Position {
  //  implicit def piecesToMap(pieces: Traversable[Piece]): Map[Piece.Location, Piece] = pieces.foldLeft(Map[Piece.Location,
  //    Piece]()) {
  //    (map: Map[Piece.Location, Piece], piece: Piece) => map.updated(piece.location, piece)
  //  }
}

case class NextPosition(previousPosition: Position, ledMovement: Piece.Movement) extends Position {

  override val pieces: Map[Piece.Location, Piece] =
    previousPosition.pieces.get(ledMovement.from) match {
      case Some(figureToMove) => (previousPosition.pieces - ledMovement.from) +
        Tuple2(ledMovement.to, figureToMove match {
          case Pawn(color) if List(1, 8).contains(ledMovement.to.row) => Queen(color)
          case something => something
        })
      case None => (previousPosition.pieces - ledMovement.from);
    }
}

import ru.zconstz.chess.Piece.tuple2Location

case object InitialPosition extends Position {

  def piecesOfColor(color: PieceColor.PieceColor) = {
    val rows = color match {
      case PieceColor.White => (1, 2)
      case PieceColor.Black => (8, 7)
    }
    Map[Piece.Location, Piece](
      (('a', rows._1): Piece.Location) -> Rook(color),
      (('b', rows._1): Piece.Location) -> Knight(color),
      (('c', rows._1): Piece.Location) -> Bishop(color),
      (('d', rows._1): Piece.Location) -> Queen(color),
      (('e', rows._1): Piece.Location) -> King(color),
      (('f', rows._1): Piece.Location) -> Bishop(color),
      (('g', rows._1): Piece.Location) -> Knight(color),
      (('h', rows._1): Piece.Location) -> Rook(color)) ++ ('a' to 'h').map(col => ((col, rows._2): Piece.Location,
      Pawn(color)))
  }

  override val pieces: Map[Piece.Location, Piece] = piecesOfColor(PieceColor.White) ++ piecesOfColor(PieceColor.Black)
}