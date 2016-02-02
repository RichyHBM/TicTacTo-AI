package players

trait Player {
  def move: (Int, Int)
  def getMarker: String
}
