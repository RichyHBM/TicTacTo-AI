package players

import game.Board


case class AiPlayer(marker: String) extends Player {
  override def move(board: Board): (Int, Int) = ???

  override def getMarker: String = marker
}
