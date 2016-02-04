package players

import game.{Game, Board}

import scala.util.Random

case class AiPlayer(marker: String) extends Player {
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
    bestMoves(Random.nextInt(bestMoves.length))
  }

  def minMax(depth: Int, board: Board, ourMove: Boolean): Int = {
    if(!board.canMove) scoreState(depth, board)
    else {
      val scoredMoves = for(avalMove <- board.allPossibleMoves) yield {
        val score = minMax(
          depth + 1,
          board.set(
            avalMove,
            if(ourMove) getMarker else getOpponentMarker ),
            !ourMove)
        (score, avalMove)
      }

      if(ourMove) scoredMoves.sortWith(_._1 > _._1).head._1
      else scoredMoves.sortWith(_._1 < _._1).head._1
    }
  }

  def scoreState(depth: Int, board: Board): Int = {
    board.winningMarker match {
      case s if s == getMarker => AiPlayer.score - depth
      case s if s == Board.empty => AiPlayer.noScore
      case _ => depth - AiPlayer.score
    }
  }

  override def getMarker: String = marker
  def getOpponentMarker: String = Game.getOppositeMarker(getMarker)
}

object AiPlayer {
  val score = 10
  val noScore = 0
}