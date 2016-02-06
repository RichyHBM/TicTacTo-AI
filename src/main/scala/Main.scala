import game.Game
import players._

object Main {
  def main(args: Array[String]) = {
    val player1 = getPlayer(Game.X)
    val player2 = getPlayer(Game.O)
    val game = new Game(player1, player2)
    game.run
  }

  //Ask user what type of player to use
  def getPlayer(marker: String) = {
    println(s"What type of player should player '$marker' be?")
    println(
      s""" 1 Input player
         | 2 Random player
         | 3 AI player
       """.stripMargin)

    scala.io.StdIn.readLine() match {
      case "1" => new InputPlayer(marker)
      case "2" => new RandomPlayer(marker)
      case "3" => {
        println("What should the max depth to check be? (0 for no max depth)")
        val inp = scala.io.StdIn.readLine().toInt
        if(inp <= 0) {
          new AiPlayer(marker)
        } else {
          new AiPlayer(marker, inp)
        }
      }
    }
  }
}
