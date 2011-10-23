package ru.zconstz.chess

import org.specs._
import ru.zconstz.chess.Position.piecesToMap
import ru.zconstz.chess.Piece.str2Location
import ru.zconstz.chess.PieceColor.char2PieceColor


class GameSpec extends Specification {

  //"game chess is played according to the rules of the game"

  "game ennded if mate is happened, whites are winners" in {
    //given("initial position")
    //when("game is played until the end within 3 turns, and whites are won")
    val whiteMovementsGen = new FakeMovementGenerator(PieceColor.White, 3)
    val blackMovementsGen = new FakeMovementGenerator(PieceColor.Black)
    val finalPosition = Game.play((whiteMovementsGen)(_), (blackMovementsGen)(_))
    //then("final position is mate and whites are winners and the last turn is #3")
    (finalPosition match {
      case MatePosition(_, PieceColor.White, 3) => true
      case _ => false
    }) must be(true)
  }

  "game ennded if mate is happened, blacks are winners" in {
    //given("initial position")
    //when("game is played until the end within 7 turns, and blacks are won")
    val whiteMovementsGen = new FakeMovementGenerator(PieceColor.White)
    val blackMovementsGen = new FakeMovementGenerator(PieceColor.Black, 7)
    val finalPosition = Game.play((whiteMovementsGen)(_), (blackMovementsGen)(_))
    //then("final position is mate and blacks are winners and the last turn is #7")
    (finalPosition match {
      case MatePosition(_, PieceColor.Black, 7) => true
      case _ => false
    }) must be(true)
  }

  "fool's mate game" in {
    //when("fool's mate game is played")
    val whiteMovementsGen = new PredefinedMovementGenerator(List(("f2", "f3"), ("g2", "g4")))
    val blackMovementsGen = new PredefinedMovementGenerator(List(("e7", "e5"), ("d8", "h4")))
    val finalPosition = Game.play(
      (whiteMovementsGen)(_),
      (blackMovementsGen)(_))
    //then("mate is registered by the engine")
    //println(finalPosition.toString)
    //println()
    //println(FoolsMateFinalPosition.toString)
    finalPosition.toString must equalTo(FoolsMateFinalPosition.toString)
    finalPosition.isCheck must be(true)
    finalPosition.isMate must be(true)
  }

  "figures are set up according to classical rules" in {
    //when("game is ready to be played")
    val position = InitialPosition
    //then("all figures of both color are in right places")
    ('a' to 'h').map(column => {
      (position(column, 2).get match {
        case Pawn(_, PieceColor.White) => true
      }) && (position(column, 7).get match {
        case Pawn(_, PieceColor.Black) => true
      })
    }).reduce(_ && _) must be(true)

    ('a' to 'h').map(column => (column, position(column, 1).get, position(column, 8).get)).map {
      posFigure =>
        posFigure match {
          case ('a', Rook(_, PieceColor.White), Rook(_, PieceColor.Black)) => true
          case ('b', Knight(_, PieceColor.White), Knight(_, PieceColor.Black)) => true
          case ('c', Bishop(_, PieceColor.White), Bishop(_, PieceColor.Black)) => true
          case ('d', Queen(_, PieceColor.White), Queen(_, PieceColor.Black)) => true
          case ('e', King(_, PieceColor.White), King(_, PieceColor.Black)) => true
          case ('f', Bishop(_, PieceColor.White), Bishop(_, PieceColor.Black)) => true
          case ('g', Knight(_, PieceColor.White), Knight(_, PieceColor.Black)) => true
          case ('h', Rook(_, PieceColor.White), Rook(_, PieceColor.Black)) => true
          case _ => fail()
        }
    }.reduce(_ && _) must be(true)
  }
}

case class MatePosition(override val previousPosition: Position, color: PieceColor.PieceColor,
                        turnNumber: Int) extends NextPosition(previousPosition, (('e', 2), ('e', 4))) {
  override def isMate = true
}

class FakeMovementGenerator(val color: PieceColor.PieceColor, val mateMovementNumber: Int = Int.MaxValue) {

  private var counter: Int = _

  def apply(pos: Position): Option[Position] = {
    counter = counter + 1
    if (counter >= mateMovementNumber) new Some(new MatePosition(pos, color, counter)) else Some(pos)
  }
}

class PredefinedMovementGenerator(var notParsedMovementsList: List[(String, String)]) {

  def apply(pos: Position): Option[Position] = {
    def locationFromString(location: String): Piece.Location = (location(0), location(1) - '0')
    def parseMovement(from: String, to: String): Piece.Movement = (locationFromString(from), locationFromString(to))
    if (!notParsedMovementsList.isEmpty)
      try {
        Some(new NextPosition(pos, parseMovement(notParsedMovementsList.head._1, notParsedMovementsList.head._2)))
      } finally {
        notParsedMovementsList = notParsedMovementsList.tail
      }
    else None
  }
}
