package game

case class Board(board: List[List[String]]) {
  require(board.forall(_.length == board.length))

  def this() = {
    this(List.fill[String](Game.boardSize, Game.boardSize)(Game.empty))
  }

  //Return a new board with all the current markers and a new marker at the given position
  def set(pos: (Int, Int), marker: String): Board = {
    require(board(pos._2)(pos._1) == " ")

    val newArr = for(y <- board.indices) yield {
      for(x <- board(y).indices) yield {
        if (x == pos._1 && y == pos._2) marker
        else getMarkerAt(x, y)
      }
    }
    new Board(newArr.map(_.toList).toList)
  }

  //Check if there are any non empty positions and there isnt a winner
  def canMove = board.exists( _.exists( _ == Game.empty) ) && !hasSomeoneWon

  //Return all empty positions
  def allPossibleMoves = {
    for{
      y <- board.indices
      x <- board(y).indices if getMarkerAt(x, y) == Game.empty
    } yield (x, y)
  }

  //If the winning marker isnt an empty space, one of the players must have won
  def hasSomeoneWon: Boolean = winningMarker != Game.empty

  //Returns the winning players marker if someone has won, or the empty marker if not
  //check if a single marker is on any of the winning combinations
  def winningMarker: String = {
    val str = for {
      l <- Board.allWinningTilesForSize(board.length)
      s <- l.map(t => getMarkerAt(t._1, t._2)).distinct if
          l.map(t => getMarkerAt(t._1, t._2)).distinct.length == 1 &&
          !l.map(t => getMarkerAt(t._1, t._2)).contains(Game.empty)
    } yield s

    if(str.toList.nonEmpty)
      str.head
    else
      Game.empty
  }

  def getBoard = board
  def getMarkerAt(x: Int, y: Int): String = board(y)(x)

  //Returns a string representation of the board in its current state
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
  //Geta all permutations (as an Int tuple) of a value and a list of values
  //I.e: 1, (1,2,3) will return (1,1), (1,2), (1,3)
  def allPerms(x: Int, y: List[Int]): List[(Int, Int)] = {
    if(y.length == 1) List((x, y.head))
    else (x, y.head) :: allPerms(x, y.tail)
  }

  //This gets all possible positions that a player must occupy to win a game
  //It gets all rows, columns, and both diagonals
  def allWinningTilesForSize(size: Int): List[List[(Int, Int)]] = {
    val ver = for(i <- 0 until size) yield allPerms(i, List.range(0, size))

    ver.toList ++
      ver.toList.map( _.map(_.swap) ) ++
      List( List.range(0, size).map(i => (i, i)) ) ++
      List( List.range(0, size).map(i => (i, (size - 1) - i)) )
  }
}
