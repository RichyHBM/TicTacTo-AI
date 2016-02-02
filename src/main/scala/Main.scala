import game.Game
import players.{AiPlayer, InputPlayer}

object Main {
  def main(args: Array[String]) = {

    println(
      s"""What type of player should player 1 be?
         |1 Input player
         |2 AI player""".stripMargin)

    val player1 = scala.io.StdIn.readLine() match {
      case "1" => new InputPlayer("X")
      case "2" => new AiPlayer("X")
    }

    println("And player 2?")

    val player2 = scala.io.StdIn.readLine() match {
      case "1" => new InputPlayer("O")
      case "2" => new AiPlayer("O")
    }

    val game = new Game(player1, player2)
    game.run
  }
}
