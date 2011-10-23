package ru.zconstz.chess

/**
 * @author Konstantin Zlobin
 */

object TwoHumansGame {

  def main(args: Array[String]) {
    Game.play(new ConsoleMovementGenerator((a, b) => true).apply(_),
      new ConsoleMovementGenerator((a, b) => true).apply(_))
  }
}