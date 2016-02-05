package game

import players.Player

case class Game(player1: Player, player2: Player) {

  //Recursively
  def doMove(currPlayer: Player, nextPlayer: Player, board: Board): Board = {
    //Clear the screen and draw the board in its current state
    print("\033[H\033[2J")
    println(board.render)
    println("Player " + currPlayer.getMarker)
    //Get the position this player wishes to place their marker at
    val playerMove = currPlayer.move(board)
    //Set the marker and get a new board with this new marker at the given position
    val newBoard = board.set(playerMove, currPlayer.getMarker)
    //If there are still available moves (and no one has won) then recurse and swap player turn
    //Else return the state of the board after having made this turn
    if(newBoard.canMove)
      doMove(nextPlayer, currPlayer, newBoard)
    else
      newBoard
  }


  def run: Unit = {
    //makes a call to fetch the final state of the game, win or draw
    val endGame = doMove(player1, player2, new Board)

    //Render the final state on a clean terminal
    print("\033[H\033[2J")
    println(endGame.render)

    //Compare the winning marker to each of the player markers and write an appropriate message
    val winningMarker = endGame.winningMarker

    if(winningMarker != Board.empty){
      println(s"Player ${winningMarker} has won!")
    } else {
      println("It was a draw!")
    }
  }
}

//Rule values for a game of Tic Tac Toe
object Game {
  val X = "X"
  val O = "O"
  def getOppositeMarker(marker: String) = {
    if(marker == X) O
    else X
  }
}
