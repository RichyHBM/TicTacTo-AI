
case class Board(board: Array[Array[String]]) {
  val rows = 3
  val cols = 3

  val boardString =
    s"""
      |${ board(0)(0) }|${ board(0)(1) }|${ board(0)(2) }
      |-+-+-
      |${ board(1)(0) }|${ board(1)(1) }|${ board(1)(2) }
      |-+-+-
      |${ board(2)(0) }|${ board(2)(1) }|${ board(2)(2) }
    """.stripMargin

  def this() = {
    this(Array.fill[String](3, 3)(" "))
  }

  def draw = {
    //print("\033[H\033[2J")
    println(boardString)
  }
}
