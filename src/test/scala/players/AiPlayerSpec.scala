package players

import game.{Game, Board}
import org.specs2.mutable.Specification

class AiPlayerSpec extends Specification {

  "AI Player" should {
    "Score win correctly" in {
      val arr = List(List(Game.X, Game.O), List(Game.X, Game.empty))
      val b = new Board(arr)
      new AiPlayer(Game.X).scoreState(0, b) must equalTo(AiPlayer.score)
      new AiPlayer(Game.X).scoreState(1, b) must equalTo(AiPlayer.score - 1)
      new AiPlayer(Game.X).scoreState(5, b) must equalTo(AiPlayer.score - 5)
    }

    "Score loss correctly" in {
      val arr = List(List(Game.X, Game.O), List(Game.O, Game.empty))
      val b = new Board(arr)
      new AiPlayer(Game.X).scoreState(0, b) must equalTo(-AiPlayer.score)
      new AiPlayer(Game.X).scoreState(1, b) must equalTo(1 - AiPlayer.score)
      new AiPlayer(Game.X).scoreState(5, b) must equalTo(5 - AiPlayer.score)
    }

    "Score tie correctly" in {
      val arr = List(List(Game.X, Game.empty), List(Game.empty, Game.O))
      val b = new Board(arr)
      new AiPlayer(Game.X).scoreState(0, b) must equalTo(AiPlayer.noScore)
    }

    "MinMax loss correctly" in {
      val arr = List(List(Game.X, Game.O), List(Game.empty, Game.O))
      val b = new Board(arr)
      new AiPlayer(Game.X).minMax(0, b, ourMove = true) must equalTo(-AiPlayer.score)
    }

    "MinMax win correctly" in {
      val arr = List(List(Game.X, Game.O), List(Game.empty, Game.empty))
      val b = new Board(arr)
      new AiPlayer(Game.X).minMax(0, b, ourMove = true) must equalTo(AiPlayer.score - 1)
    }
  }
}
