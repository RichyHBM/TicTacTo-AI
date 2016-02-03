package game


case class Board(board: Array[Array[String]]) {
  require(board.forall(_.length == board.length))

  def this() = {
    this(Array.fill[String](Board.boardSize, Board.boardSize)(Board.empty))
  }

  def set(pos: (Int, Int), marker: String) = {
    require(board(pos._2)(pos._1) == " ")
    board(pos._2)(pos._1) = marker
  }

  def canMove = board.exists( _.exists( _ == Board.empty) ) && !hasSomeoneWon

  def hasSomeoneWon: Boolean = winningMarker != Board.empty

  def winningMarker: String = {
    val str = for {
      l <- Board.allWinningTilesForSize(board.length)
      s <- l.map(t => getMarkerAt(t._1, t._2)).distinct if
          l.map(t => getMarkerAt(t._1, t._2)).distinct.length == 1 &&
          !l.map(t => getMarkerAt(t._1, t._2)).contains(Board.empty)
    } yield s

    if(str.toList.nonEmpty)
      str.head
    else
      Board.empty
  }

  def getBoard = board
  def getMarkerAt(x: Int, y: Int): String = board(y)(x)

  def render : String = {
    val b = new StringBuilder

    b.append(" ")
    for (c <- board.indices)
      b.append(" " + c)
    b.append("\n")
    for(r <- board.indices) {
      b.append(r + " ")
      for (c <- board(r).indices) {
        b.append(getMarkerAt(c, r))
        if(c < board(r).length - 1)
          b.append("|")
      }
      b.append("\n")
      if(r < board.length - 1) {
        b.append("  ")
        for(_ <- 0 until board(r).length - 1) b.append("-+")
        b.append("-\n")
      }
    }
    b.mkString
  }
}

object Board {

  def allPerms(x: Int, y: List[Int]): List[(Int, Int)] = {
    if(y.length == 1) List((x, y.head))
    else (x, y.head) :: allPerms(x, y.tail)
  }

  val boardSize = 3
  val empty = " "

  def allWinningTilesForSize(size: Int): List[List[(Int, Int)]] = {
    val ver = for(i <- 0 until size) yield allPerms(i, List.range(0, size))

    ver.toList ++
      ver.toList.map( _.map(_.swap) ) ++
      List( List.range(0, size).map(i => (i, i)) ) ++
      List( List.range(0, size).map(i => (i, (size - 1) - i)) )
  }
}
