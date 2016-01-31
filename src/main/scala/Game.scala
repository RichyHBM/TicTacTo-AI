
case class Game(player1: Player, player2: Player) {
  val board = new Board

  def canMove: Boolean = false

  def run: Unit = {
    while(canMove && !player1.hasWon && !player2.hasWon) {
      board.set(player1.move, player1.getMarker)

      board.draw

      if(!player1.hasWon)
        board.set(player2.move, player2.getMarker)

      board.draw
    }
  }
}
