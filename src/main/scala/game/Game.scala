package game

import players.Player

case class Game(player1: Player, player2: Player) {

  def doMove(currPlayer: Player, nextPlayer: Player, board: Board): Board = {
    print("\033[H\033[2J")
    println(board.render)
    println("Player " + currPlayer.getMarker)
    val playerMove = currPlayer.move(board)
    val newBoard = board.set(playerMove, currPlayer.getMarker)
    if(newBoard.canMove)
      doMove(nextPlayer, currPlayer, newBoard)
    else
      newBoard
  }

  def run: Unit = {
    val endGame = doMove(player1, player2, new Board)

    print("\033[H\033[2J")
    println(endGame.render)

    if(endGame.winningMarker == player1.getMarker){
      println("Player 1 has won!")
    }else if(endGame.winningMarker == player2.getMarker) {
      println("Player 2 has won!")
    } else {
      println("No one won!")
    }
  }
}
