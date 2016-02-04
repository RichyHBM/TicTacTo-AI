package players

import game.Board

trait Player {
  def move(board: Board): (Int, Int)
  def getMarker: String
}
