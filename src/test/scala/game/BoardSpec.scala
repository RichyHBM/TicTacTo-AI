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
        val arr = List(List(Game.X, Board.empty), List(Board.empty, Game.O))
        val b = new Board(arr)
        b.render must equalTo(
          "  0 1\n" +
          s"0 ${Game.X}|${Board.empty}\n" +
          "  -+-\n" +
          s"1 ${Board.empty}|${Game.O}\n")
      }

      "Fetch correct marker" in {
        val arr = List(List(Game.X, Game.O), List(Board.empty, Game.O))
        val b = new Board(arr)
        b.getMarkerAt(0, 0) must equalTo(Game.X)
        b.getMarkerAt(1, 0) must equalTo(Game.O)
        b.getMarkerAt(0, 1) must equalTo(Board.empty)
        b.getMarkerAt(1, 1) must equalTo(Game.O)
      }

      "Set marker correctly" in {
        val arr = List(List(Game.X, Board.empty), List(Board.empty, Game.O))
        val b = new Board(arr)
        val newB = b.set((1, 0), Game.X)
        newB.getMarkerAt(0, 0) must equalTo(Game.X)
        newB.getMarkerAt(1, 0) must equalTo(Game.X)
        newB.getMarkerAt(0, 1) must equalTo(Board.empty)
        newB.getMarkerAt(1, 1) must equalTo(Game.O)
      }

      "Be able to allow next move" in {
        val arr = List(List(Game.X, Board.empty), List(Board.empty, Game.O))
        val b = new Board(arr)
        b.canMove must equalTo(true)
      }

      "Not be able to allow next move when full" in {
        val arr = List(
          List(Game.X, Game.O, Game.X),
          List(Game.X, Game.O, Game.X),
          List(Game.O, Game.X, Game.O)
        )
        val b = new Board(arr)
        b.canMove must equalTo(false)
      }

      "Not be able to allow next move when won" in {
        val arr = List(List(Game.X, Game.O), List(Game.X, Board.empty))
        val b = new Board(arr)
        b.canMove must equalTo(false)
      }

      "Determine someone has won" in {
        val arr = List(List(Game.X, Game.O), List(Game.X, Board.empty))
        val b = new Board(arr)
        b.hasSomeoneWon must equalTo(true)

        val arr2 = List(List(Game.X, Game.O), List(Game.O, Board.empty))
        val b2 = new Board(arr2)
        b2.hasSomeoneWon must equalTo(true)
      }

      "Determine correct marker when won" in {
        val arr = List(List(Game.X, Game.O), List(Game.X, Board.empty))
        val b = new Board(arr)
        b.winningMarker must equalTo(Game.X)

        val arr2 = List(List(Game.O, Game.O), List(Game.X, Board.empty))
        val b2 = new Board(arr2)
        b2.winningMarker must equalTo(Game.O)
      }

      "Not allow non equal sizes" in {
        val arr = List(List(Game.X, Board.empty), List(Game.X, Board.empty))
        val b = new Board(arr)
        val moves = b.allPossibleMoves
        moves must contain( (1,0) )
        moves must contain( (1,1) )
      }

      "Not allow non equal sizes" in {
        val arr = List(List(Game.X, Game.O, Board.empty), List(Game.X, Board.empty, Board.empty))
        try {
          val b = new Board(arr)
          true must equalTo(false)
        }catch {
          case _: Throwable => true must equalTo(true)
        }
      }

      "Not allow moving in non-empty space" in {
        val arr = List(List(Game.X, Game.O), List(Game.X, Board.empty))
        val b = new Board(arr)
        try {
          b.set((0, 0), Game.O)
          true must equalTo(false)
        }catch {
          case _: Throwable => true must equalTo(true)
        }
      }
    }
}
