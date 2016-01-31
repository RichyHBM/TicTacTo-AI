
case class Board(board: Array[Array[String]]) {
  require(board.length == Consts.rows)
  require(board.forall(_.length == Consts.cols))

  def this() = {
    this(Array.fill[String](Consts.rows, Consts.cols)(" "))
  }

  def set(pos: (Int, Int), marker: String) = {
    require(board(pos._2)(pos._1) == " ")
    board(pos._2)(pos._1) = marker
  }

  def draw = {
    print("\033[H\033[2J")

    for(r <- 0 to Consts.rows - 1) {
      print(" ")
      for (c <- 0 to Consts.cols - 1) {
        print(board(r)(c))
        if(c < Consts.cols - 1)
          print("|")
      }
      println()
      if(r < Consts.rows - 1)
        println(" -+-+- ")
    }
  }
}
