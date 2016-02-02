package game

import players.Player

case class Game(player1: Player, player2: Player) {
  val board = new Board

  def run: Unit = {
    while(board.canMove) {
      print("\033[H\033[2J")
      println(board.render)
      println("Player 1")
      board.set(player1.move, player1.getMarker)

      print("\033[H\033[2J")

      if(board.canMove){
        println(board.render)
        println("Player 2")
        board.set(player2.move, player2.getMarker)
      }
    }
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
