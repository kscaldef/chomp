package chomp

object Main extends App {
  println("Let's play chomp\n")

  var board = Board.initial(3,4).display

  while (!board.gameOver) {
    board = userMove(board)

    if (!board.gameOver) {
      board = computerMove(board)
    }
  }

  def userMove(board: Board): Board = {
    val move = Move.input
    println
    board.makeMove(move).display
  }

  def computerMove(board: Board): Board = {
    val (move, smiley) = AI.negamax(board) match {
      case Left(m) => (m, ":-)")
      case Right(Some(m)) => (m, ":-P")
      case Right(None) => (Move(0,0), ":-(")
    }
    println("> %s %s\n".format(move, smiley))

    board.makeMove(move).display
  }
}

case class Move(row: Int, col: Int) {
  // for consistency with input
  override def toString = "%s %s".format(row+1,col+1)
}

object Move {
  val InputRx = """(\d+) (\d+)""".r
  def input: Move = {
    print("? ")
    val line = readLine
    line match {
      case InputRx(r,c) => Move(r.toInt-1,c.toInt-1)
    }
  }
}

case class Board(rows: Seq[Seq[Boolean]]) {
  def nRows = rows.size
  def nCols = rows(0).size
  def gameOver: Boolean = !rows(0)(0)

  def display: Board = {
    println(rows.map { r => r.map { c => if (c) "o" else "." }.mkString("") ++ "\n"}.mkString(""))
    this
  }

  // TODO, clean this up
  def makeMove(move: Move): Board = {
    def helper(row: Int, col: Int, rows: Seq[Seq[Boolean]]): Seq[Seq[Boolean]] = (row, rows) match {
      case (_, Nil) => Nil
      case (0, rs) => alterCols(col, rs)
      case (n, r :: rs) => r +: helper(n-1, col, rs)
    }
    def alterCols(col:Int, rows: Seq[Seq[Boolean]]): Seq[Seq[Boolean]] = {
      def alterCol(col: Int, vals: Seq[Boolean]): Seq[Boolean] = (col, vals) match {
        case (_, Nil) => Nil
        case (0, v :: vs) => false +: alterCol(0, vs)
        case (c, v :: vs) => v +: alterCol(c-1, vs)
      }

      rows.map { r => alterCol(col, r) }
    }

    Board(helper(move.row, move.col, rows))
  }
}

object Board {
  def initial(nRows: Int, nCols: Int) = Board(Seq.fill(nRows, nCols)(true))
}

object AI {
  // returns either a winning move, or a possible losing move
  def negamax(b: Board): Either[Move, Option[Move]] = {
    val validMoves = for {
      r <- 0 until (b.nRows)
      c <- 0 until (b.nCols)
      if (b.rows(r)(c) && (r > 0 || c > 0))
    } yield { Move(r,c) }

    validMoves find { m => negamax(b.makeMove(m)).isRight } match {
      case Some(m) => Left(m)
      case None => Right(validMoves.lastOption)
    }
  }
}
