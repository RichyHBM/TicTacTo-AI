
trait Player {
  def move: (Int, Int)
  def hasWon: Boolean
  def getMarker: String
}
