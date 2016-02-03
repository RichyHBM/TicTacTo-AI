package game

import org.specs2.mutable.Specification

class BoardSpec extends Specification {
  def checkWinningPos(size: Int) = {
    val wins = Board.allWinningTilesForSize(size)

    for(y <- 0 until size) {
      val win = for(x <- 0 until size) yield (x, y)
      wins must contain(win)
      wins must contain(win.map(_.swap))
    }

    val vertical = for(i <- 0 until size) yield (i, i)
    val vertical2 = for(i <- 0 until size) yield (i, (size - 1) - i)
    wins must contain(vertical)
    wins must contain(vertical2)

    wins.length must equalTo(size * 2 + 2)
  }

    "Board" should {
      "Give all perms" in {
        val i = 1
        val arr = List(1, 2)
        val perms = Board.allPerms(i, arr)
        perms must contain((1, 1))
        perms must contain((1, 2))
        perms.length must equalTo(2)
      }

      "Give all winning positions, size 2" in {
        checkWinningPos(2)
      }

      "Give all winning positions, size 3 - 9" in {
        for(i <- 3 to 9)
          checkWinningPos(i)

        true must equalTo(true)
      }

      "Render correctly" in {
        val arr = Array(Array("X", " "), Array(" ", "O"))
        val b = new Board(arr)
        b.render must equalTo(
          "  0 1\n" +
          "0 X| \n" +
          "  -+-\n" +
          "1  |O\n")
      }

      "Fetch correct marker" in {
        val arr = Array(Array("X", "O"), Array(" ", "O"))
        val b = new Board(arr)
        b.getMarkerAt(0, 0) must equalTo("X")
        b.getMarkerAt(1, 0) must equalTo("O")
        b.getMarkerAt(0, 1) must equalTo(" ")
        b.getMarkerAt(1, 1) must equalTo("O")
      }

      "Set marker correctly" in {
        val arr = Array(Array("X", " "), Array(" ", "O"))
        val b = new Board(arr)
        b.set((1, 0), "X")
        b.getMarkerAt(0, 0) must equalTo("X")
        b.getMarkerAt(1, 0) must equalTo("X")
        b.getMarkerAt(0, 1) must equalTo(" ")
        b.getMarkerAt(1, 1) must equalTo("O")
      }

      "Be able to allow next move" in {
        val arr = Array(Array("X", " "), Array(" ", "O"))
        val b = new Board(arr)
        b.canMove must equalTo(true)
      }

      "Not be able to allow next move when full" in {
        val arr = Array(
          Array("X", "O", "X"),
          Array("X", "O", "X"),
          Array("O", "X", "O")
        )
        val b = new Board(arr)
        b.canMove must equalTo(false)
      }

      "Not be able to allow next move when won" in {
        val arr = Array(Array("X", "O"), Array("X", " "))
        val b = new Board(arr)
        b.canMove must equalTo(false)
      }

      "Determine someone has won" in {
        val arr = Array(Array("X", "O"), Array("X", " "))
        val b = new Board(arr)
        b.hasSomeoneWon must equalTo(true)

        val arr2 = Array(Array("X", "O"), Array("O", " "))
        val b2 = new Board(arr2)
        b2.hasSomeoneWon must equalTo(true)
      }

      "Determine correct marker when won" in {
        val arr = Array(Array("X", "O"), Array("X", " "))
        val b = new Board(arr)
        b.winningMarker must equalTo("X")

        val arr2 = Array(Array("O", "O"), Array("X", " "))
        val b2 = new Board(arr2)
        b2.winningMarker must equalTo("O")
      }

      "Not allow non equal sizes" in {
        val arr = Array(Array("X", " "), Array("X", " "))
        val b = new Board(arr)
        val moves = b.allPossibleMoves
        moves must contain( (1,0) )
        moves must contain( (1,1) )
      }

      "Not allow non equal sizes" in {
        val arr = Array(Array("X", "O", " "), Array("X", " ", " "))
        try {
          val b = new Board(arr)
          true must equalTo(false)
        }catch {
          case _: Throwable => true must equalTo(true)
        }
      }

      "Not allow moving in non-empty space" in {
        val arr = Array(Array("X", "O"), Array("X", " "))
        val b = new Board(arr)
        try {
          b.set((0, 0), "O")
          true must equalTo(false)
        }catch {
          case _: Throwable => true must equalTo(true)
        }
      }
    }
}
