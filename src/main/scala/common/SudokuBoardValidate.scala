package common

object SudokuBoardValidate extends  App {

  val board =
  Seq(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
      Seq(4, 5, 6, 7, 8, 9, 3, 2, 1),
      Seq(7, 8, 9, 1, 2, 3, 4, 5, 6))

  case class Coordinates(row: Int, col: Int)

  class ValidateBoard(board: Seq[Seq[Int]]) {

    val currentPosition = Coordinates(0, -1)
    val totalElements = board.size * board.head.size
    val maxColumns = board.head.size
    val maxRows = board.size
    var hashMap = Map.empty[Int, Coordinates]

    @scala.annotation.tailrec
    final def checkForBoardValidity(totalElements: Int = totalElements, currentPosition: Coordinates = currentPosition, isValid: Boolean = true,
                                    acc: Map[Int, Coordinates] =  Map.empty[Int, Coordinates]): Boolean = {
      if(!isValid) isValid
      else if(totalElements == 0)  isValid
      else {
        if(currentPosition.col < maxColumns - 1) {
          val newPos = moveForward(currentPosition)
          val value = board(newPos.row)(newPos.col)
          if (acc.get(value).isEmpty)
            checkForBoardValidity(totalElements - 1, currentPosition = newPos, acc = acc ++ Map(value -> newPos))
          else if (acc(value).col == newPos.col)
            checkForBoardValidity(totalElements - 1, currentPosition = newPos, isValid = false, acc = acc)
          else
            checkForBoardValidity(totalElements - 1, currentPosition = newPos, acc = acc)
        }
        else if(currentPosition.row < maxRows - 1) {
          val newPos =  moveDown(currentPosition)
          val value = board(newPos.row)(newPos.col)
          if (acc.get(value).isEmpty)
            checkForBoardValidity(totalElements - 1, currentPosition = newPos, acc = acc ++ Map(value -> newPos))
          else if (acc(value).row == newPos.row)
            checkForBoardValidity(totalElements - 1, currentPosition = newPos, isValid = false, acc = acc)
          else
            checkForBoardValidity(totalElements - 1, currentPosition = newPos, acc = acc)
        } else {
          isValid
        }
      }

    }

    def moveDown(currPosition: Coordinates): Coordinates = {
      val newPosition = Coordinates(currPosition.row + 1, 0)
      println(board(newPosition.row)(newPosition.col))
      newPosition
    }

    def moveForward(currPosition: Coordinates): Coordinates = {
      val newPosition = Coordinates(currPosition.row, currPosition.col + 1)
      println(board(newPosition.row)(newPosition.col))
      newPosition
    }
  }

  val results = new ValidateBoard(board).checkForBoardValidity()
  if(results) println("is valid board!!")
  else println("is not a valid board!!!")

}
