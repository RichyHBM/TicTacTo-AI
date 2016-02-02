package players


case class AiPlayer(marker: String) extends Player {
  override def move: (Int, Int) = ???

  override def getMarker: String = marker
}
