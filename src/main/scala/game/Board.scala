package game


case class Board(board: Array[Array[String]]) {
  require(board.length == Board.boardSize)
  require(board.forall(_.length == Board.boardSize))

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
      l <- Board.allWinningTiles
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
    for (c <- 0 until Board.boardSize)
      b.append(" " + c)
    b.append("\n")
    for(r <- 0 until Board.boardSize) {
      b.append(r + " ")
      for (c <- 0 until Board.boardSize) {
        b.append(board(r)(c))
        if(c < Board.maxBoardPos)
          b.append("|")
      }
      b.append("\n")
      if(r < Board.maxBoardPos) {
        b.append("  ")
        for(_ <- 0 until Board.maxBoardPos) b.append("-+")
        b.append("- \n")
      }
    }
    b.mkString
  }
}

object Board {

  private def allPerms(x: Int, y: List[Int]): List[(Int, Int)] = {
    if(y.length == 1) List((x, y.head))
    else (x, y.head) :: allPerms(x, y.tail)
  }

  val boardSize = 3
  val maxBoardPos = boardSize - 1
  val empty = " "

  val allWinningTiles: List[List[(Int, Int)]] = {
    val ver = for(i <- 0 until boardSize) yield allPerms(i, List.range(0, boardSize))

    ver.toList ++
      ver.toList.map( _.map(_.swap) ) ++
      List( List.range(0, boardSize).map(i => (i, i)) ) ++
      List( List.range(0, boardSize).map(i => (i, maxBoardPos - i)) )
  }
}
