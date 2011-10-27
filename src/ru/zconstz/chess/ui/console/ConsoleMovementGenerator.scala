package ru.zconstz.chess.ui.console

import ru.zconstz.chess.{NextPosition, Position, Piece}
import ru.zconstz.chess.Piece.str2Location

/**
 * @author Konstantin Zlobin
 */

class ConsoleMovementGenerator(val movementValidator: (Position, Piece.Movement) => Boolean) extends {

  def apply(position: Position): Option[Position] = {
    println("here is position: ")
    println(position.toString())
    var userMovement: Piece.Movement = null
    do {
      userMovement = readUserInput() match {
        case None => return None
        case Some(movement) => movement
      }
    } while (!movementValidator(position, userMovement))
    Some(NextPosition(position, userMovement))
  }

  def readUserInput(): Option[Piece.Movement] = {
    var line = ""
    do {
      line = readLine("enter valid movement or ! for termination:")
    } while (!line.matches("[a-h][1-8]\\-[a-h][1-8]") && line.trim() != "!")
    line match {
      case "!" => None
      case movement => Some(movement.split('-')(0): Piece.Location, movement.split('-')(1): Piece.Location);
    }
  }
}