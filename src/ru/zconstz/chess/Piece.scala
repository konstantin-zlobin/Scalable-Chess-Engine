package ru.zconstz.chess

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
  type Movement = (Location, Location)
  type MovementTemplate = (Int, Int)
  type Location = (Char, Int)

  implicit def str2Location(str: String): Location = (str.charAt(0), str(1) - '0')
}

case class Piece(location: Piece.Location, color: PieceColor.PieceColor, weight: Int) {

  def diagonals(maxDist: Int = 7): Seq[Piece.MovementTemplate] = (1 to maxDist).map(n => List[Piece.MovementTemplate]((n, n), (-n, -n), (n, -n), (-n, n))).flatten

  def horizontalsAndVerticals(maxDist: Int = 7): Seq[Piece.MovementTemplate] = (1 to maxDist).map(n => List[Piece.MovementTemplate]((0, n), (0, -n), (n, 0), (-n, 0))).flatten

  def allStraitDirections(maxDist: Int = 7): Seq[Piece.MovementTemplate] = diagonals(maxDist) ++ horizontalsAndVerticals(maxDist)

  lazy val possibleMovementTemplates: Seq[Piece.MovementTemplate] = List()
}

case class King(override val location: Piece.Location, override val color: PieceColor.PieceColor) extends Piece(location, color, Int.MaxValue) {
  override lazy val possibleMovementTemplates = allStraitDirections(1)
}

case class Queen(override val location: Piece.Location, override val color: PieceColor.PieceColor) extends Piece(location, color, 8) {
  override lazy val possibleMovementTemplates = allStraitDirections()
}

case class Rook(override val location: Piece.Location, override val color: PieceColor.PieceColor) extends Piece(location, color, 5) {
  override lazy val possibleMovementTemplates = horizontalsAndVerticals()
}

case class Bishop(override val location: Piece.Location, override val color: PieceColor.PieceColor) extends Piece(location, color, 3) {
  override lazy val possibleMovementTemplates = diagonals()
}

case class Knight(override val location: Piece.Location, override val color: PieceColor.PieceColor) extends Piece(location, color, 3) {
  override lazy val possibleMovementTemplates = List(1, -1).combinations(2).map(multiplier =>
    List(1, 2).permutations.map(basement => (basement(0) * multiplier(0), basement(1) * multiplier(1)))).flatten.toSeq
}

case class Pawn(override val location: Piece.Location, override val color: PieceColor.PieceColor) extends Piece(location, color, 1) {
  override lazy val possibleMovementTemplates = color match {
    case PieceColor.White => Seq((0, 1), (0, 2), (1, 1), (-1, 1))
    case PieceColor.Black => Seq((0, -1), (0, -2), (1, -1), (-1, -1))
  }
}
