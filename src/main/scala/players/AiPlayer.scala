package players

import game.{Game, Board}
import scala.util.Random

case class AiPlayer(marker: String) extends Player {
  //Computes the MinMax score for each of the available moves,
  // and returns the one with the highest score
  override def move(board: Board): (Int, Int) = {
    val default = (Integer.MIN_VALUE, Seq((0, 0)))
    val bestMoves = board.
      allPossibleMoves.
      groupBy(m => minMax(0, board.set(m, getMarker), ourMove = false)).
      fold[(Int, Seq[(Int, Int)])](default)((acc, e) => {
      acc match {
        case ac if ac._1 > e._1 => ac
        case _ => e
      }
    })._2
    //If there are multiple moves with the same score (eg, if this is the first turn) select one at random
    bestMoves(Random.nextInt(bestMoves.length))
  }

  //Computes a score for a board state based on whose turn it is and how far down the tree it is
  def minMax(depth: Int, board: Board, ourMove: Boolean): Int = {
    //Once there are no more moves that can be made, score the current state of the board
    if(!board.canMove) scoreState(depth, board)
    else {
      //For all moves possible in this board, score the min max,
      // increasing the depth and swapping whose turn it would be
      val scoredMoves = for(avalMove <- board.allPossibleMoves) yield {
        val score = minMax(
          depth + 1,
          board.set(
            avalMove,
            if(ourMove) getMarker else getOpponentMarker ),
            !ourMove)
        (score, avalMove)
      }
      //Depending on whose move it currently is, return the highest or lowest score
      if(ourMove) scoredMoves.sortWith(_._1 > _._1).head._1
      else scoredMoves.sortWith(_._1 < _._1).head._1
    }
  }

  //Scores a board based on its state, and how many turns into the future it is
  //The sooner we can get a board into a state where we win, the higher the score
  def scoreState(depth: Int, board: Board): Int = {
    board.winningMarker match {
      case s if s == getMarker => AiPlayer.score - depth
      case s if s == Board.empty => AiPlayer.noScore
      case _ => depth - AiPlayer.score
    }
  }

  //Gets this players marker, and the opositions marker
  override def getMarker: String = marker
  def getOpponentMarker: String = Game.getOppositeMarker(getMarker)
}

//Values to use when assessing the score of a move
object AiPlayer {
  val score = 10
  val noScore = 0
}
