
object FindShortestSubString extends App {

  def findShortestSubString(s: String): Long = {
    var sub_strings = collection.mutable.Map[Char, Int]()
    var maxSize = 0
    var currSize = 0
    for(c <- s.toSeq) {
      val char = c.toLower
      if(sub_strings.get(char).isEmpty) {
        sub_strings += (char -> 1)
        currSize = sub_strings.size
      }
      else {
        if(currSize > maxSize) maxSize = currSize
        sub_strings = collection.mutable.Map[Char, Int]()
      }
    }
    if(maxSize == 0) currSize else maxSize
  }

  println(findShortestSubString("BBBaaabac"))

}



