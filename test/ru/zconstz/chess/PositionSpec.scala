package ru.zconstz.chess

import org.specs.Specification
import ru.zconstz.chess.Position.piecesToMap
import ru.zconstz.chess.Piece.str2Location
import ru.zconstz.chess.PieceColor.char2PieceColor

/**
 * @author Konstantin Zlobin
 */

class PositionSpec extends Specification {

  "position is considered a mate if" in {
    val pos = ExplicitlyDefinedPosition(List(King("a1", 'w'), King("h8", 'b'), Queen("h2", 'b'), Rook("h1", 'b')))
    pos.isMate must be(true)
  }

  "another position - fool's mate is considered a mate" in {
    FoolsMateFinalPosition.isCheck must be(true)
    FoolsMateFinalPosition.isMate must be(true)
  }

  "position is considered a check if" in {
    val pos = ExplicitlyDefinedPosition(List(King("a1", 'w'), King("h8", 'b'), Rook("h1", 'b')))
    pos.isCheck must be(true)
    pos.isMate must be(false)
  }

  "legal movements of the king" in {
    val pos = ExplicitlyDefinedPosition(List(King("a1", 'w'), Pawn("a2", 'w')))
    val kingMovements = pos.pieceLegalMovements(pos.pieces("a1"))
    kingMovements.size must be(2)
    kingMovements mustContain "b1"
    kingMovements mustContain "b2"
  }

  "next position defined by base position and movement" in {
    val basePosition = ExplicitlyDefinedPosition(List(King("e1", 'w'), King("e8", 'b'), Queen("d8", 'b')))
    val newPosition = NextPosition(basePosition, ("e1": Piece.Location, "e2": Piece.Location))
    newPosition("e1") must equalTo(None)
    newPosition("e2") match {
      case Some(King(location, color)) => location must equalTo("e2": Piece.Location)
      case _ => fail()
    }
  }
}

case class ExplicitlyDefinedPosition(override val pieces: Map[Piece.Location, Piece]) extends Position

object FoolsMateFinalPosition extends ExplicitlyDefinedPosition(
  List(Rook("a1", 'w'), Knight("b1", 'w'), Bishop("c1", 'w'), Queen("d1", 'w'), King("e1", 'w'),
    Bishop("f1", 'w'), Knight("g1", 'w'), Rook("h1", 'w')) ++
    List(Pawn("a2", 'w'), Pawn("b2", 'w'), Pawn("c2", 'w'), Pawn("d2", 'w'),
      Pawn("e2", 'w'), Pawn("f3", 'w'), Pawn("g4", 'w'), Pawn("h2", 'w')) ++
    List(Rook("a8", 'b'), Knight("b8", 'b'), Bishop("c8", 'b'), Queen("h4", 'b'), King("e8", 'b'),
      Bishop("f8", 'b'), Knight("g8", 'b'), Rook("h8", 'b')) ++
    List(Pawn("a7", 'b'), Pawn("b7", 'b'), Pawn("c7", 'b'), Pawn("d7", 'b'),
      Pawn("e5", 'b'), Pawn("f7", 'b'), Pawn("g7", 'b'), Pawn("h7", 'b'))
)