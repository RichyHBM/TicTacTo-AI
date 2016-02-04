import game.Game
import players.{AiPlayer, InputPlayer}

object Main {
  def main(args: Array[String]) = {

    println(
      s"""What type of player should player '${Game.X}' be?
         |1 Input player
         |2 AI player""".stripMargin)

    val player1 = scala.io.StdIn.readLine() match {
      case "1" => new InputPlayer(Game.X)
      case "2" => new AiPlayer(Game.X)
    }

    println(s"And player '${Game.O}'?")

    val player2 = scala.io.StdIn.readLine() match {
      case "1" => new InputPlayer(Game.O)
      case "2" => new AiPlayer(Game.O)
    }

    val game = new Game(player1, player2)
    game.run
  }
}
