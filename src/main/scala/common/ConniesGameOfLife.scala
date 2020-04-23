package common

object ConniesGameOfLife extends App {

  val board =     Seq(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
    Seq(4, 5, 6, 4, 8, 9, 3, 2, 1),
    Seq(7, 8, 9, 1, 2, 3, 4, 5, 6))


  def gameOfLife(board: Array[Array[Int]]): Unit = {
    case class Coordinates(row: Int, col: Int)
    val totalRows = board.toSeq.size
    val totalCols = board.toSeq.head.size
    val totalPeople = totalRows * totalCols

    val startingPerson = Coordinates(0, 0)

    var nextGenBoard: Array[Array[Int]] = board

    traverseBoard(startingPerson, totalPeople)

    @scala.annotation.tailrec
    def traverseBoard(currentPerson: Coordinates, totalElements: Int = totalPeople): Unit = {
      if(totalElements == 0) {

        nextGenBoard.foreach(println)

      } else {

        val numberOfAlivePeople = numberOfNeighboursLive(currentPerson)

        println(currentPerson.row + " <- R C -> " + currentPerson.col)
        println("alive neighbours -> " + numberOfAlivePeople)

        val boardCellType = board(currentPerson.row)(currentPerson.col)

        println("cell type -> " + boardCellType)

        val cellValue =
          if(boardCellType == 1) {
            if(numberOfAlivePeople < 2 || numberOfAlivePeople > 3) 0
            else if(numberOfAlivePeople == 2 || numberOfAlivePeople == 3) 1
            else 0
          } else {
            numberOfAlivePeople match {
              case a if a == 3 => 1
              case _ => 0
            }
          }

        println("cell value -> " + cellValue)
        println("---------------------------")

        nextGenBoard(currentPerson.row)(currentPerson.col) = cellValue

        if(currentPerson.col < totalCols - 1) {
          val newPosition = moveForward(currentPerson)
          traverseBoard(newPosition, totalPeople - 1)
        } else if(currentPerson.row < totalRows - 1) {
          val newPosition = moveDown(currentPerson)
          traverseBoard(newPosition, totalPeople - 1)
        } else {
          println(" Done ")
        }
      }
    }

    def moveForward(currentPostion: Coordinates): Coordinates =
      Coordinates(currentPostion.row, currentPostion.col + 1)

    def moveDown(currentPosition: Coordinates): Coordinates =
      Coordinates(currentPosition.row + 1, 0)


    def numberOfNeighboursLive(currentPerson: Coordinates): Int = {


      def isThereAnAliveNeighbourAboveMe(): Boolean =
        if(currentPerson.row > 0) board(currentPerson.row - 1)(currentPerson.col) == 1
        else false

      def isThereAnAliveNeighbourBelowMe(): Boolean =
        if(currentPerson.row < totalRows - 1) board(currentPerson.row + 1)(currentPerson.col) == 1
        else false

      def isThereAnAliveNeighbourBehind(): Boolean =
        if(currentPerson.col > 1) board(currentPerson.row)(currentPerson.col - 1) == 1
        else false

      def isThereAnAliveNeighbourAccros(): Boolean = {
        if(currentPerson.col < totalCols - 1) board(currentPerson.row)(currentPerson.col + 1) == 1
        else false
      }

      def isAliveNeighbourDiagnalInBottomLeft(): Boolean = {
        if(currentPerson.col > 1 && currentPerson.row < totalRows - 1) board(currentPerson.row + 1)(currentPerson.col - 1) == 1
        else false
      }

      def isAliveNeighbourDiagnalInBottomRight(): Boolean = {
        if(currentPerson.col < totalCols - 1 && currentPerson.row < totalRows - 1) board(currentPerson.row + 1)(currentPerson.col + 1) == 1
        else false
      }

      def isAliveNeighbourDiagnalInTopRight(): Boolean = {
        if(currentPerson.col < totalCols - 1 && currentPerson.row > 1) board(currentPerson.row - 1)(currentPerson.col + 1) == 1
        else false
      }

      def isAliveNeighbourDiagnalInTopLeft(): Boolean = {
        println(currentPerson)
        if(currentPerson.col > 1 && currentPerson.row > 0) board(currentPerson.row - 1)(currentPerson.col - 1) == 1
        else false
      }

      val isAliveAboveMe = isThereAnAliveNeighbourAboveMe()
      val isAliveBelowMe = isThereAnAliveNeighbourBelowMe()
      val isAliveBehindMe = isThereAnAliveNeighbourBehind()
      val isAliveAccrossMe = isThereAnAliveNeighbourAccros()

      val isAliveTopLeft = isAliveNeighbourDiagnalInTopLeft()
      val isAliveTopRight = isAliveNeighbourDiagnalInTopRight()
      val isAliveBottomRight = isAliveNeighbourDiagnalInBottomRight()
      val isAliveBottomLeft = isAliveNeighbourDiagnalInBottomLeft()

      Seq(isAliveAboveMe, isAliveBelowMe,
        isAliveBehindMe, isAliveAccrossMe,
        isAliveTopLeft, isAliveTopRight,
        isAliveBottomRight, isAliveBottomLeft).count(_ == true)
    }
  }

}
