package players

import game.Board

case class InputPlayer(marker: String) extends Player {
  override def move(board: Board): (Int, Int) = {
    println("Where would you like to go? 'X,Y'")
    val line = scala.io.StdIn.readLine()
    val pos = line.split(",")
    require(pos.length == 2)
    (pos.head.toInt, pos.tail.head.toInt)
  }
  
  override def getMarker: String = marker
}
