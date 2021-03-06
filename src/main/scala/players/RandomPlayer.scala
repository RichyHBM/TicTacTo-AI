package players

import game.Board
import scala.util.Random

case class RandomPlayer(marker: String) extends Player {
  //Select one of the available moves at random
  override def move(board: Board): (Int, Int) = {
    val moves = board.allPossibleMoves.toList
    moves(Random.nextInt(moves.length))
  }

  override def getMarker: String = marker
}
