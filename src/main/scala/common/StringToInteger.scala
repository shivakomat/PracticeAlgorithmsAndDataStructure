package common

object StringToInteger extends  App {

  def stringToInteger(string: String): Int = {

    val characters = string.toCharArray.toSeq

    def getInteger(chars: Seq[Char], integersOnly: Seq[Char] = Seq.empty[Char]): Seq[Char] =
      if (chars.isEmpty) {
        integersOnly
      }
      else {
        val currentChar = chars.head
        val integersOnlyList =
          if (currentChar.isDigit || (currentChar == '-' && chars.take(2).last.isDigit)) integersOnly ++ Seq(chars.head)
          else integersOnly
        getInteger(chars.tail, integersOnlyList)
      }

    val integersOnly = getInteger(characters)
    integersOnly.mkString.toInt
  }


  println(stringToInteger("-4193"))
  println(stringToInteger("words with -4193"))
  println(stringToInteger("4193 with words"))
  println(stringToInteger("4193 with -words"))

}