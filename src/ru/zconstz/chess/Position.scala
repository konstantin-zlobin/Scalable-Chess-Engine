package ru.zconstz.chess

/**
 * @author Konstantin Zlobin
 */

abstract class Position {
  val pieces: Map[Piece.Location, Piece]

  def isCheck: Boolean = whatHappenToKings(isKingUnderAttackByPiece(_, _, _))

  def isMate: Boolean = whatHappenToKings((king, piece, movementTemplate) => isKingUnderAttackByPiece(king, piece,
    movementTemplate) && pieceLegalMovements(king).isEmpty)

  def weight: Map[PieceColor.PieceColor, Int] =
    pieces.foldLeft(Map[PieceColor.PieceColor, Int](PieceColor.White -> 0, PieceColor.Black -> 0)) {
      (result, piece) => piece._2.color match {
        case PieceColor.White => Map(PieceColor.White -> (result(PieceColor.White) + piece._2.weight), PieceColor.Black -> result(PieceColor.Black))
        case PieceColor.Black => Map(PieceColor.White -> result(PieceColor.White), PieceColor.Black -> (result(PieceColor.Black) + piece._2.weight))
      }
    }

  def apply(location: Piece.Location): Option[Piece] = pieces.get(location)

  def locationByTemplate(location: Piece.Location, movementTemplate: Piece.MovementTemplate): Piece.Location =
    ((location._1 + movementTemplate._1).toChar, location._2 + movementTemplate._2)

  def pieceLegalMovements(piece: Piece): Seq[Piece.Location] = {
    def isNotPossiblePawnMovement(movement: Piece.MovementTemplate): Boolean = movement._2 == 2 && piece.location._2 != 2 || movement._2 == -2 && piece.location._2 != 7

    def isNotPossibleKingMovement(movement: Piece.MovementTemplate): Boolean = allFiguresOfOppositeColor(piece.color)
      .find(p => p.possibleMovementTemplates.find(t => locationByTemplate(p.location,
      t) == locationByTemplate(piece.location, movement)).isDefined).isDefined

    def isOutOfBoardBoundaries(locationToCheck: Piece.Location) = locationToCheck._1 < 'a' || locationToCheck._1 > 'h' || locationToCheck._2 < 1 || locationToCheck._2 > 8

    def placeIsBusyWithPieceOfSameColorSaveCastlingCase(locationToCheck: Piece.Location) = {
      pieces.get(locationToCheck) match {
        case None => false;
        case Some(pieceAtLocation) => pieceAtLocation.color == piece.color &&
          !(pieceAtLocation.isInstanceOf[King] && piece.isInstanceOf[Rook]) &&
          !(pieceAtLocation.isInstanceOf[Rook] && piece.isInstanceOf[King])
      }
    }

    piece.possibleMovementTemplates
      .filterNot(movement => piece match {
      case Pawn(_, _) => isNotPossiblePawnMovement(movement)
      case King(_, _) => isNotPossibleKingMovement(movement)
      case _ => false
    }).map(movement => locationByTemplate(piece.location, movement))
      .filterNot(newPosition => isOutOfBoardBoundaries(newPosition) || placeIsBusyWithPieceOfSameColorSaveCastlingCase(newPosition))
  }

  def whatHappenToKings(predicate: (King, Piece, Piece.MovementTemplate) => Boolean): Boolean = {
    def kings = pieces.values.filter(p => p.isInstanceOf[King]).asInstanceOf[Iterable[King]]
    def findCheckMovementsOf(piece: Piece) = piece.possibleMovementTemplates.filter(movementTemplate => kings.find
      (predicate(_, piece, movementTemplate)).isDefined)
    pieces.values.find(piece => !findCheckMovementsOf(piece).isEmpty).isDefined
  }

  def isKingUnderAttackByPiece(king: King, piece: Piece, movementTemplate: Piece.MovementTemplate): Boolean = {
    king.location == locationByTemplate(piece.location, movementTemplate) && king.color != piece.color
  }

  def allFiguresOfColor(color: PieceColor.PieceColor): Iterable[Piece] = pieces.values.filter(_.color == color)

  def allFiguresOfOppositeColor(color: PieceColor.PieceColor): Iterable[Piece] = pieces.values.filterNot(_.color == color)

  import ru.zconstz.chess.PieceColor.pieceColor2String

  override def toString: String =
    (for (row <- (1 to 8)) yield (for (col <- ('a' to 'h')) yield (pieces.get((col, row)) match {
      case Some(King(_, color)) => "K" + (color: String)
      case Some(Queen(_, color)) => "Q" + (color: String)
      case Some(Rook(_, color)) => "R" + (color: String)
      case Some(Bishop(_, color)) => "B" + (color: String)
      case Some(Knight(_, color)) => "N" + (color: String)
      case Some(Pawn(_, color)) => "P" + (color: String)
      case None => "  "
    })).mkString(row + "-[", ",", "]")).mkString(System.lineSeparator())

}

object Position {
  implicit def piecesToMap(pieces: Traversable[Piece]): Map[Piece.Location, Piece] = pieces.foldLeft(Map[Piece.Location,
    Piece]()) {
    (map: Map[Piece.Location, Piece], piece: Piece) => map.updated(piece.location, piece)
  }
}

case class NextPosition(previousPosition: Position, ledMovement: Piece.Movement) extends Position {

  override val pieces: Map[Piece.Location, Piece] =
    previousPosition(ledMovement._1) match {
      case Some(figureToMove) => (previousPosition.pieces - ledMovement._1) +
        Tuple2(ledMovement._2, figureToMove match {
          case King(_, color) => King(ledMovement._2, color)
          case Queen(_, color) => Queen(ledMovement._2, color)
          case Rook(_, color) => Rook(ledMovement._2, color)
          case Bishop(_, color) => Bishop(ledMovement._2, color)
          case Knight(_, color) => Knight(ledMovement._2, color)
          case Pawn(_, color) if List(1, 8).contains(ledMovement._2._2) => Queen(ledMovement._2, color)
          case Pawn(_, color) => Pawn(ledMovement._2, color)
        })
      case None => (previousPosition.pieces - ledMovement._1);
    }
}

case object InitialPosition extends Position {

  def piecesOfColor(color: PieceColor.PieceColor) = {
    val rows = color match {
      case PieceColor.White => (1, 2)
      case PieceColor.Black => (8, 7)
    }
    List(Rook(('a', rows._1), color),
      Knight(('b', rows._1), color),
      Bishop(('c', rows._1), color),
      Queen(('d', rows._1), color),
      King(('e', rows._1), color),
      Bishop(('f', rows._1), color),
      Knight(('g', rows._1), color),
      Rook(('h', rows._1), color)) ++ ('a' to 'h').map(col => Pawn((col, rows._2), color))
  }

  import Position.piecesToMap

  override val pieces: Map[Piece.Location, Piece] = piecesOfColor(PieceColor.White) ++ piecesOfColor(PieceColor.Black)
}