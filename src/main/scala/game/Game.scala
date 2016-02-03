package game

import players.Player

case class Game(player1: Player, player2: Player) {
  val board = new Board

  def doMove(currPlayer: Player, nextPlayer: Player): Unit = {
    print("\033[H\033[2J")
    println(board.render)
    println("Player " + currPlayer.getMarker)
    board.set(currPlayer.move, currPlayer.getMarker)
    if(board.canMove)
      doMove(nextPlayer, currPlayer)
  }

  def run: Unit = {
    doMove(player1, player2)

    print("\033[H\033[2J")
    println(board.render)

    if(board.winningMarker == player1.getMarker){
      println("Player 1 has won!")
    }else if(board.winningMarker == player2.getMarker) {
      println("Player 2 has won!")
    } else {
      println("No one won!")
    }
  }
}
