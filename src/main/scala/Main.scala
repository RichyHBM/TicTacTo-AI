import game.Game
import players.{RandomPlayer, AiPlayer, InputPlayer}

object Main {
  def main(args: Array[String]) = {
    val player1 = getPlayer(Game.X)
    val player2 = getPlayer(Game.O)
    val game = new Game(player1, player2)
    game.run
  }
  
  def getPlayer(marker: String) = {
    println(s"What type of player should player '$marker' be?")
    val allPlayers = getAllPlayers(marker)
    for(i <- allPlayers.indices)
      println(s" $i ${allPlayers(i).getClass.getSimpleName}")

    val input = scala.io.StdIn.readLine()
    allPlayers(input.toInt)
  }

  def getAllPlayers(marker: String) = {
    List(
      new InputPlayer(marker),
      new RandomPlayer(marker),
      new AiPlayer(marker)
    )
  }
}
