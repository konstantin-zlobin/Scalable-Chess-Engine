package ru.zconstz.chess

/**
 * @author Konstantin Zlobin
 */

object Game {


  //  var currentPosition: Position = InitialPosition
  //
  //  def initBoard() {
  //    currentPosition = InitialPosition
  //  }

  //  def checkMovement() {
  //
  //  }
  //
  //  def humanMovement(pos: Position) = pos(checkMovement(parseMovement(User.newMovement(pos))))
  //
  //  def computerMovement(color: PieceColor.PieceColor, pos: Position) {
  //    val newPositions: List[Position] = for (val piece <- pos.getAllFiguresOfColor(color)) {
  //      val movements = pieceLegalMovements(piece)
  //      List[Position] = for (val movement <- movements; newPosition = position(movement)) yield newPosition
  //    }
  //    newPositions.reduceLeft((a, b) => (if (a.weight > b.weight) a else b))
  //  }

  def play(whiteMovementGen: (Position) => Option[Position], blackMovementGen: (Position) => Option[Position],
           position: Position = InitialPosition): Position = {
    val pAfterWhite = whiteMovementGen(position) match {
      case Some(newPosition) if !position.isMate=> newPosition
      case _ => return position
    }
    val pAfterBlack = blackMovementGen(pAfterWhite) match {
      case Some(newPosition) if !position.isMate=> newPosition
      case _ => return position
    }
    play(whiteMovementGen, blackMovementGen, pAfterBlack)
  }


  def main(args: Array[String]) {
    //    initBoard()


    //    Random.shuffle(board.board.flatten.filter(_ != null).filter(piece => piece.asInstanceOf[Piece].color == PieceColor.White)).foreach {
    //      piece => {
    //        val aFigure = piece.asInstanceOf[Piece]
    //        //pieceLegalMovements()
    //      }
    //    }
  }
}

//object Board {
//  val a = 1
//  val b = 2
//  val c = 3
//  val d = 4
//  val e = 5
//  val f = 6
//  val g = 7
//  val h = 8
//
//  type Position = (Int, Int)
//
//  var board: Array[Array[Piece]] = Array.ofDim[Piece](8, 8)
//
//  var blackFigures = List()
//
//  var whiteFigures = List()
//
//  def getFigureByPosition(pos: Position): Piece = board(pos._1 - 1)(pos._2 - 1)
//
//  def setFigureToPosition(pos: Position, figure: Piece) {
//    board(pos._1 - 1)(pos._2 - 1) = figure
//    //    piece.position = (pos._1 - 1, pos._2 - 1)
//  }
//
//  def clear() {
//    board = Array.ofDim[Piece](8, 8)
//  }
//
//  def setupFigures() {
//    PieceColor.values.foreach(placeFiguresOfGivenColorToInitialPositionsOnBoard _)
//  }
//
//  private def placeFiguresOfGivenColorToInitialPositionsOnBoard(color: PieceColor.PieceColor) {
//    val rowNums = color match {
//      case PieceColor.White => (1, 2)
//      case PieceColor.Black => (8, 7)
//    }
//    //    List((a, Rook(color)), (b, Knight(color)), (c, Bishop(color)), (d, Queen(color)),
//    //      (e, King(color)), (f, Bishop(color)), (g, Knight(color)), (h, Rook(color)))
//    //      .foreach(columnAndFigure => setFigureToPosition((columnAndFigure._1, rowNums._1), columnAndFigure._2))
//    //    (a to h).foreach(column => setFigureToPosition((column, rowNums._2), Pawn(color)))
//  }
//}
