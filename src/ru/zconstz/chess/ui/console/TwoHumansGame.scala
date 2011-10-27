package ru.zconstz.chess.ui.console

import ru.zconstz.chess.Game

/**
 * @author Konstantin Zlobin
 */

object TwoHumansGame {

  def main(args: Array[String]) {
    Game.play(new ConsoleMovementGenerator((a, b) => true).apply(_),
      new ConsoleMovementGenerator((a, b) => true).apply(_))
  }
}